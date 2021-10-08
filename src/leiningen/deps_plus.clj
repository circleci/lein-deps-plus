(ns leiningen.deps-plus
  (:refer-clojure :exclude [list])
  (:require [leiningen.core.main :as lein]
            [circleci.deps-plus.core :as deps]
            [clojure.java.io :as io]
            [leiningen.core.project :as project]
            [clojure.string :as str]))

(defn- list
  "Lists all dependencies of the current project."
  [project]
  (deps/write-dependency-list project *out*))

(defn- list-save
  "Save a list of all dependencies of the current project."
  [project]
  (with-open [out (io/writer "deps.list")]
    (deps/write-dependency-list project out)))

(defn- list-diff
  "Compare current dependencies to those previously saved with `list-save`."
  [project]
  (let [in (io/file "deps.list")]
    (when-not (.exists in)
      (lein/warn "No deps.list file found. list-save must be run before list-diff.")
      (lein/exit 1))
    (deps/diff-dependency-list project (slurp in) *out*)))

(defn- check-downgraded
  "Checks for dependencies which have been downgraded.
  A dependency is considered \"downgraded\" if the resolved version is older
  than the version specified by some other included dependency. This can occur
  if `:managed-dependencies` is used to pin a dependency version or if Maven
  conflict resolution happens to not select the latest version."
  [project]
  (doseq [[dep max-version] (deps/check-downgraded* project)]
    (lein/info (format "%s downgraded to %s"
                 (assoc dep :version max-version)
                 (:version dep)))))

(defn- check-families
  "Checks for inconsistent dependency versions within families.
  Dependencies families are sets of dependencies which share a common group ID
  and version. Inconsistent family versions can occur if the `:managed-dependencies`
  for a family are incomplete or inconsistent. This situation can also occur
  when different family members are brought in through different transitive
  dependency paths. To fix this issue consider adding a complete set of
  `:managed-dependencies` entries for the family."
  [project]
  (when-let [combos (seq (deps/check-families* project))]
    (lein/info (format "Found %d suspicious combinations." (count combos)))
    (doseq [[parent child] combos]
      (lein/info "Suspicious combination," (str parent) "and" (str child)))))

(defn- format-dependency-tree-node
  "Formats the dependency in Leiningen style."
  [dep]
  (let [dep (deps/coordinates->aether dep)]
    ;; A Leiningen dependency is formatted as,
    ;;
    ;;   [group/artifact "version" :param-key "param-value" ...]
    ;;
    ;; We rely on the fact that unquoted and quoted values alternate
    ;; to convert to a string representation.
    (str "[" (->> (partition 2 dep)
                  (map #(apply format "%s \"%s\"" %))
                  (str/join " ")) "]")))

(defn- print-dependency-tree
  "Prints a human readable dependency tree.
  `deps` should be a dependency tree encoded as a map from coordinates to
  collections of direct dependency coordinates. If provided, `resolved` should
  be a set of resolved dependency coordinates. Matching dependencies will then
  be annotated in the dependency graph output."
  [deps resolved]
  (let [;; Build a sequence of tree paths in depth first order (i.e. the order
        ;; we need to print them in). Each path element is a `[node is-last?]`
        ;; tuple where "is-last?" indicates if the node is the last child of
        ;; its parent node.
        paths (tree-seq
                (constantly true)
                (fn [path]
                  (let [[node] (peek path)
                        children (get deps node)
                        last-idx (dec (count children))]
                    (->> (map (fn [child idx] [child (= idx last-idx)])
                              children (range))
                         (map #(conj path %)))))
                [[nil true]])
        ;; Every path begins with the nil root node.
        ;; Remove it to simplify output.
        paths (->> (map #(subvec % 1) paths)
                   (remove empty?))]
    (doseq [path paths]
      (let [[node last?] (peek path)
            prefix-parts (concat
                           (->> (pop path)
                                (map second)
                                (map (fn [last?] (if last? "   " "|  "))))
                           [(if last? "\\- " "+- ")])
            prefix (apply str prefix-parts)
            annotated? (some #(deps/same-dependency? % node) resolved)
            version-range (:version-range (meta node))
            premanaged-version (or (:premanaged-version (meta node))
                                 (:version node))
            suffix (cond
                     ;; Dependency should not be annotated.
                     (not annotated?)
                     ""

                     ;; Version was omitted by conflict resolution.
                     (not (resolved node))
                     " (omitted)"

                     ;; Version was resolved from version range.
                     version-range
                     (str " (chosen from range " version-range ")")

                     ;; Version was chosen by :managed-dependencies.
                     (not= (str premanaged-version) (str (:version node)))
                     (str " (managed from " premanaged-version ")")

                     ;; Version matches the version chosen.
                     :else " (chosen)")]
        (lein/info (str "   ;; " prefix (format-dependency-tree-node node) suffix))))))

(defn- check-pedantic
  "Checks for dependency conflicts and shows suggested :managed-dependencies.
  Version conflicts occur when dependency resolution selects a version of a
  dependency that is older than one appearing elsewhere in the dependency
  graph (e.g. clj-time 0.14.0 is chosen at the exclusion of 0.15.0). Conflicts
  are also reported any time a version range is encountered in the dependency
  graph.

  The output from this task is a set of suggested managed dependencies. For
  each managed dependency a dependency tree is also output that shows how the
  conflicting dependency is depended upon. To suppress the outputting of these
  trees pass the \":quiet\" flag.

  This check is similar to Leiningen's `:pedantic? :abort` mode, but shows
  suggested :managed-dependencies instead of :exclusions. Unlike Leiningen,
  plugin dependencies are ignored by this task."
  [project & args]
  (when-let [conflicts (seq (sort-by str (deps/find-pedantic-conflicts project)))]
    (lein/info (format "Found %d dependency conflicts." (count conflicts)))
    (lein/info "Considering adding the following :managed-dependencies,")
    (doseq [[dep conflict-tree] conflicts]
      (when-not (some #{":quiet"} args)
        (lein/info "")
        (print-dependency-tree conflict-tree #{dep}))
      (lein/info (format "   [%s/%s \"%s\"]"
                   (:group-id dep) (:artifact-id dep) (:version dep))))))

(defn- check-management-conflicts
  "Checks for dependency management conflicts.
  A dependency management conflict occurs when a dependency has versions
  specified in both :dependencies and :managed-dependencies."
  [project]
  (when-let [conflicts (seq (deps/check-management-conflicts* project))]
    (lein/info (format "Found %d management conflicts." (count conflicts)))
    (doseq [[dep managed] conflicts]
      (lein/info (format "%s conflicts with managed dependency %s"
                         dep managed)))))

(defn- check-classpath-conflicts
  "Checks for classpath conflicts.
  Classpath conflicts occur when resources which should be unique (e.g. Java
  class files, Clojure namespaces, etc) are found in multiple dependencies.
  Classpath conflicts can lead to unpredictable behavior at runtime as the
  version of the resource which is loaded can depend on subtle factors like
  the ordering of JARs on the classpath, or the order in which JARs are merged
  into an uberjar. Classpath conflicts are often caused by dependencies being
  duplicated (e.g. two versions of a dependency with different Maven
  coordinates) or dependencies which bundle other dependencies (e.g. shaded
  JARs). In these cases the conflict can be resolved by replacing the offending
  dependency with a more appropriate one (e.g. a non-shaded version)."
  [project]
  (let [conflicts (deps/check-classpath-conflicts* project)
        ;; Index conflicts by the sets of dependencies they appear in
        conflicts-by-set (->> (group-by val conflicts)
                              (reduce-kv
                                (fn [acc k entries]
                                  (assoc acc k (into [] (map key entries))))
                                {}))
        format-dep-set (fn [dep-set]
                         (let [deps (into [] (sort (map str dep-set)))]
                           (str (str/join ", " (pop deps)) " and " (peek deps))))
        ;; Pre-format the dependency set keys as strings so we can
        ;; sort the output and provide a stable output order.
        conflicts-by-set-str (reduce-kv
                               (fn [acc dep-set conflicts]
                                 (assoc acc
                                   (format-dep-set dep-set)
                                   conflicts))
                               {}
                               conflicts-by-set)]
    (when-not (empty? conflicts)
      (lein/info (format "Found %d classpath conflicts between %d dependencies."
                   (count conflicts)
                   (->> conflicts-by-set keys (reduce into) count)))
      (doseq [[dep-set-str conflicts] (sort conflicts-by-set-str)]
        (lein/info "")
        (lein/info (format "Found %d classpath conflicts between %s"
                     (count conflicts) dep-set-str))
        (let [n 10]
          (doseq [entry (take n (sort conflicts))]
            (lein/info " " entry))
          (when (< n (count conflicts))
            (lein/info " " (- (count conflicts) n) "more...")))))))

(defn- parse-target-dependency [^String target]
  (if (.contains target ":")
    (deps/parse-coordinates target)
    (deps/aether->coordinates [(symbol target) nil])))

(defn- why
  "Show all paths to the given dependency as a tree.
  This is similar to `lein deps :why`, but all copies and paths to the given
  dependency are shown."
  [project group-name]
  (let [target (parse-target-dependency group-name)
        target (->> (deps/resolve-dependencies project)
                    (keys)
                    (filter #(deps/same-dependency? % target))
                    (first))]
    (if target
      (let [subtree (deps/resolve-targeted-dependency-subtree project target)]
        (print-dependency-tree subtree #{target}))
      (lein/warn "No dependency found matching" group-name))))

(defn- who-shades
  "Show project dependencies which shade the given dependency."
  [project group-name]
  (let [target (parse-target-dependency group-name)]
    (doseq [[dep target-version] (deps/who-shades* project target)]
      (println (str dep) "shades" (str target-version)))))

(defn- error [project message]
  (lein/warn "[ERROR]" message "\n")
  (lein/resolve-and-apply project ["help" "deps-plus"])
  (lein/exit 1 message))

(defn- with-managed-deps
  "Run a Leiningen task with the projects managed dependencies used as its dependencies."
  [project args]
  (let [dependencies (->> (:managed-dependencies project)
                          (map (fn [[name _ & rest]]
                                 (into [name nil] rest))))
        project (project/merge-profiles project [{:dependencies dependencies}])]
    (lein/resolve-and-apply project args)))

(defn deps-plus
  "Extra tasks for analyzing dependencies"
  {:subtasks [#'list #'list-save #'list-diff #'check-downgraded #'check-families #'check-pedantic
              #'check-management-conflicts #'check-classpath-conflicts #'why #'who-shades]}
  [project & [sub-task & args]]
  (let [project (project/unmerge-profiles project [:base :user])]
    (case sub-task
      "list" (list project)
      "list-save" (list-save project)
      "list-diff" (list-diff project)
      "check-downgraded" (check-downgraded project)
      "check-families" (check-families project)
      "check-pedantic" (apply check-pedantic project args)
      "check-management-conflicts" (check-management-conflicts project)
      "check-classpath-conflicts" (check-classpath-conflicts project)
      "why" (apply why project args)
      "who-shades" (apply who-shades project args)
      "with-managed-deps" (with-managed-deps project args)
      ;; Leiningen is supposed to translate "lein <task> help" into
      ;; "lein help <task>" by default. For some reason it does not
      ;; seem to be working.
      "help" (lein/resolve-and-apply project (concat ["help" "deps-plus"] args))
      nil (error project "Task required.")
      (error project "Unknown task."))))
