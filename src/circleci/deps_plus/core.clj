(ns circleci.deps-plus.core
  (:require [leiningen.core.classpath :as lein-classpath]
            [cemerick.pomegranate.aether :as aether]
            [circleci.deps-plus.graph :as graph]
            [clojure.set :as set]
            [clojure.string :as string]
            [leiningen.core.main :as lein])
  (:import (org.eclipse.aether DefaultRepositorySystemSession)
           (org.eclipse.aether.graph DependencyNode)
           (org.eclipse.aether.artifact Artifact)
           (org.apache.maven.artifact.versioning ComparableVersion)
           (org.eclipse.aether.collection CollectResult)
           (java.io Writer File)
           (java.util.jar JarFile)
           (java.util Properties)
           (org.eclipse.aether.util.graph.manager DependencyManagerUtils)
           (org.eclipse.aether.resolution DependencyResult)))

(def dependencies-key :dependencies)
(def managed-dependencies-key :managed-dependencies)

(defrecord Coordinates [group-id artifact-id classifier version extension]
  Object
  (toString [this]
    (->> [group-id artifact-id extension classifier (str version) (:scope (meta this))]
         (remove empty?)
         (string/join ":"))))

(defn parse-coordinates [s]
  (let [[group-id artifact-id ext x y z] (string/split s #":")
        ext (or ext "jar")]
    (cond
      (some? z)
      (let [classifier x
            version (ComparableVersion. y)
            scope z]
        (with-meta
          (->Coordinates group-id artifact-id classifier version ext)
          {:scope scope}))

      (#{"compile" "provided" "runtime" "test"} y)
      (let [version (ComparableVersion. x)
            scope y]
        (with-meta
          (->Coordinates group-id artifact-id "" version ext)
          {:scope scope}))

      (some? y)
      (let [classifier x
            version (ComparableVersion. y)]
        (->Coordinates group-id artifact-id classifier version ext))

      (some? x)
      (let [version (ComparableVersion. x)]
        (->Coordinates group-id artifact-id "" version ext))

      :else
      (->Coordinates group-id artifact-id "" nil ext))))

(defn- artifact-coordinates [^Artifact a]
  (when a
    (->Coordinates
      (.getGroupId a)
      (.getArtifactId a)
      (.getClassifier a)
      (some-> (.getVersion a) (ComparableVersion.))
      (.getExtension a))))

(defn- node-coordinates [^DependencyNode n]
  (when-let [coord (artifact-coordinates (.getArtifact n))]
    (with-meta coord
      {:scope              (.getScope (.getDependency n))
       :version-range      (some-> n .getVersionConstraint .getRange)
       :premanaged-version (DependencyManagerUtils/getPremanagedVersion n)
       :premanaged-scope   (DependencyManagerUtils/getPremanagedScope n)
       :file               (some-> n .getArtifact .getFile)})))

(defn coordinates->aether [{:keys [group-id artifact-id classifier version extension] :as dep}]
  (let [scope (:scope (meta dep))]
    (concat [(symbol group-id artifact-id) (str version)]
      (when-not (string/blank? classifier) [:classifier classifier])
      (when-not (#{"jar" nil} extension) [:extension extension])
      (when-not (#{"compile" nil} scope) [:scope scope]))))

(defn aether->coordinates [[group-artifact version & {:keys [scope classifier extension]}]]
  (with-meta
    (->Coordinates
      (or (namespace group-artifact) (name group-artifact))
      (name group-artifact)
      (or classifier "")
      (some-> version ComparableVersion.)
      (or extension "jar"))
    {:scope scope}))

(defn unversioned [coordinates]
  (assoc coordinates :version nil))

(defn- basic-repository-session
  "Returns a basic repository session that enables verbose dependency
  management property tracking (pre-managed version, scope, etc)."
  ^DefaultRepositorySystemSession [& args]
  (doto ^DefaultRepositorySystemSession (apply aether/repository-session args)
    (.setConfigProperty DependencyManagerUtils/CONFIG_PROP_VERBOSE true)))

(defn- full-dependency-graph-repository-session
  "Returns a repository session without a dependency graph transformer.
  Without a dependency graph transformer the full dependency graph will
  be returned by the dependency collector."
  [& args]
  (doto (apply basic-repository-session args)
    (.setDependencyGraphTransformer nil)))

(defn- dependency-graph [^DependencyNode root]
  (loop [edges {} to-visit [root] visited #{root}]
    (if (seq to-visit)
      (let [[node & rest] to-visit
            coordinates (node-coordinates node)
            child-nodes (seq (.getChildren node))
            child-coordinates (into #{} (map node-coordinates child-nodes))]
        (recur
          (update edges coordinates set/union child-coordinates)
          (into rest (filter #(not (visited %)) child-nodes))
          (into visited child-nodes)))
      edges)))

(defn- transfer-listener [e]
  (let [{:keys [type] {:keys [name repository]} :resource} e]
    (when (= :started type)
      (locking *err*
        (lein/warn "Retrieving" name "from" repository)))))

(defn resolve-dependencies [project & flags]
  (let [{:keys [repositories local-repo mirrors offline?]} project
        flags (into #{} flags)
        managed-coordinates (get project managed-dependencies-key)
        coordinates (-> (get project dependencies-key)
                        (aether/merge-versions-from-managed-coords managed-coordinates))
        args (concat
               [:local-repo local-repo
                :repositories (into {} (map lein-classpath/add-repo-auth repositories))
                :coordinates coordinates
                :mirrors (map lein-classpath/add-repo-auth mirrors)
                :proxy (lein-classpath/get-proxy-settings)
                :retrieve (boolean (:retrieve flags))
                :transfer-listener transfer-listener
                :repository-session-fn (if (:full-graph flags)
                                         full-dependency-graph-repository-session
                                         basic-repository-session)
                :offline? offline?]

               (when-not (:ignore-managed-dependencies flags)
                 [:managed-coordinates managed-coordinates]))
        result (apply aether/resolve-dependencies* args)
        root (if (:retrieve flags)
               (.getRoot ^DependencyResult result)
               (.getRoot ^CollectResult result))]
    (dependency-graph root)))

(defn- resolve-natural-dependencies
  "Returns a graph obtained by merging the full \"natural\" dependency graphs
  for each dependency specified by the given coordinates. Since this is a full
  dependency graph, cyclic dependencies, version conflicts, and duplicate nodes
  may be present. This graph can be useful for comparing the dependencies declared
  by a given dependency to those resolved for use by the project."
  [project coordinates]
  (let [aether-coordinates (->> coordinates
                                (map coordinates->aether)
                                (into []))
        alt-project (merge project
                      {:dependencies         aether-coordinates
                       :managed-dependencies nil})]
    (resolve-dependencies alt-project :full-graph)))

(defn- max*
  "Returns the greatest value as ordered by `compare`."
  [values]
  (reduce
    (fn [acc x]
      (if (> (compare x acc) 0) x acc))
    values))

(defn check-downgraded*
  "Returns a sequence of downgraded dependencies as described by `check-downgraded`.
  Each downgraded dependency is returned as a pair, `[dependency max-version]`."
  [project]
  (let [resolved-deps (->> (resolve-dependencies project)
                           (keys)
                           (remove nil?)
                           (into #{}))

        ;; Get the full dependency graph obtained by depending directly on all of our
        ;; resolved dependencies without managed dependencies in place. This allows us
        ;; to compare the transitive dependencies that each of our dependencies would
        ;; bring by default, to those we get with dependency management in place.
        natural-dep-graph (resolve-natural-dependencies project resolved-deps)
        natural-dep-versions (->> (keys natural-dep-graph)
                                  (remove nil?)
                                  (group-by unversioned)
                                  (reduce-kv (fn [m k v]
                                               (assoc m k (map :version v)))
                                    {}))

        max-versions (reduce-kv
                       (fn [m k v] (assoc m k (max* v)))
                       {}
                       natural-dep-versions)]

    (for [dep (sort-by str resolved-deps)
          :let [version (:version dep)
                max-version (get max-versions (unversioned dep))]
          :when (< (compare version max-version) 0)]
      [dep max-version])))

(defn check-families*
  "Returns a sequence of questionable `[child parent]` dependencies as
  described by `check-families`."
  [project]
  (let [deps (->> (resolve-dependencies project)
                  (keys)
                  (remove nil?)
                  (into #{}))

        versions (->> deps
                      (map (fn [dep] [(unversioned dep) dep]))
                      (into {}))

        natural-deps (resolve-natural-dependencies project deps)

        same-family? (fn [a b]
                       (let [keys [:group-id :version]]
                         (= (select-keys a keys) (select-keys b keys))))]

    (for [dep (sort-by str deps)
          ;; Find direct dependencies of dep which, in the natural dependency graph,
          ;; are in part of the same family as dep (same group-id and version). For
          ;; each of these family members, lookup the actual version resolved.
          :let [family-members (->> (get natural-deps dep)
                                    (filter #(same-family? dep %))
                                    (map unversioned)
                                    (map #(get versions % (format "%s (excluded)" %))))]
          member (sort-by str family-members)
          :when (not= (:version member) (:version dep))]
      [dep member])))

(defn write-dependency-list
  [project ^Writer writer]
  (doseq [dep (->> (resolve-dependencies project)
                   (keys)
                   (remove nil?)
                   (map str)
                   (sort))]
    (.write writer (str dep "\n")))
  (.flush writer))

(defn- parse-dependency-list [s]
  (when (seq s)
    (->> (string/split-lines s)
         (map parse-coordinates))))

(defn- diff-categorise
  [^Coordinates old-dep ^Coordinates new-dep]
  (let [old-scope (:scope (meta old-dep))
        old-version (:version old-dep)
        new-scope (:scope (meta new-dep))
        new-version (:version new-dep)]
    (cond
      (not old-dep)
      [:added (format "%s" new-dep)]

      (not new-dep)
      [:removed (format "%s" old-dep)]

      (> (compare old-version new-version) 0)
      [:downgraded (format "%s (%s -> %s)" new-dep old-version new-version)]

      (< (compare old-version new-version) 0)
      [:upgraded (format "%s (%s -> %s)" new-dep old-version new-version)]

      (not= old-scope new-scope)
      [:rescoped (format "%s (%s -> %s)" new-dep old-scope new-scope)])))

(defn diff-dependency-list
  [project old-list ^Writer writer]
  (let [index #(->> (group-by unversioned %)
                    (reduce-kv
                     (fn [m k v]
                       (assoc m k (first v)))
                     {}))
        old (->> (parse-dependency-list old-list)
                 (index))
        new (->> (resolve-dependencies project)
                 (keys)
                 (remove nil?)
                 (index))
        all (into #{} (concat (keys old) (keys new)))
        groups (->> (sort-by str all)
                    (keep #(diff-categorise (get old %) (get new %)))
                    (group-by first))]
    (doseq [category [:added :removed :upgraded :downgraded :rescoped]
            :when (contains? groups category)
            :let [changes (category groups)]]
      (.write writer (format ";; %s\n" (string/capitalize (name category))))
      (doseq [change changes]
        (.write writer (format "%s\n" (second change))))
      (.write writer "\n")))
  (.flush writer))

(defn same-dependency?
  "Returns true if the dependencies `a` and `b` are equal ignoring version."
  [a b]
  (= (unversioned a) (unversioned b)))

(defn- apply-managed-dependency
  "Applies a managed dependency to a dependency graph.
  Matching dependencies are updated to point to the managed version of
  the dependency. The dependency graph is then pruned of no longer
  reachable dependencies."
  [deps managed-dep]
  (let [managed-dep (vary-meta managed-dep dissoc :version-range)
        replace #(if (same-dependency? managed-dep %) managed-dep %)
        deps (reduce-kv
               (fn [acc dep directs]
                 (assoc acc
                   (if (same-dependency? managed-dep dep)
                     (vary-meta dep dissoc :version-range)
                     dep)
                   (into #{} (map replace directs))))
               {}
               deps)
        reachable (graph/graph-seq #(get deps %) nil)]
    (select-keys deps reachable)))

(defn- pedantic-conflict-check [resolved-dep all-versions]
  (let [v (:version resolved-dep)
        max-v (max* (map :version all-versions))]
    (or (< (compare v max-v) 0)
        (some #(:version-range (meta %)) all-versions))))

(defn- extract-targeted-subtree
  "Extracts the minimal subtree from `full-graph` that captures all paths from
  the nil root node to `target`."
  [full-graph target]
  (->> (graph/graph-path-seq #(get full-graph %) nil)
       (filter #(same-dependency? (peek %) target))
       (mapcat #(partition 2 1 %))
       (group-by first)
       (reduce-kv
         (fn [acc node children]
           (assoc acc node (into #{} (map second children))))
         {})))

(defn- find-first-conflict
  "Find the first conflicting dependency in the given dependency graph.
  The dependency returned is the first dependency found via a breadth
  first search having conflicts."
  [full-graph resolved conflict-check]
  (let [;; For "all-versions" we look at the map values (i.e. the right-hand
        ;; side of each graph edge) instead of the map keys, because the
        ;; values preserve metadata, including :version-range information.
        all-versions (->> (vals full-graph)
                          (mapcat identity)
                          (remove nil?)
                          (group-by unversioned))
        resolved-versions (reduce
                            (fn [acc dep]
                              (assoc acc (unversioned dep) dep))
                            {}
                            resolved)
        conflict (->> (graph/graph-seq #(get full-graph %) nil)
                      (remove nil?)
                      (map unversioned)
                      (filter #(conflict-check
                                 (resolved-versions %)
                                 (all-versions %)))
                      (map resolved-versions)
                      (first))]
    (when conflict
      [conflict (extract-targeted-subtree full-graph conflict)])))

(defn find-pedantic-conflicts
  "Finds dependency version conflicts.
  For each conflict, returns a vector containing the chosen version of the dependency,
  and a minimal dependency tree containing all the paths to the conflicting dependency."
  [project]
  (let [resolved (->> (resolve-dependencies project)
                      (keys)
                      (remove nil?))]
    (loop [conflicts [] full-graph (resolve-dependencies project :full-graph)]
      (if-let [conflict (find-first-conflict full-graph resolved pedantic-conflict-check)]
        (recur (conj conflicts conflict)
               (apply-managed-dependency full-graph (first conflict)))
        conflicts))))

(defn check-management-conflicts*
  "Checks for conflicts between dependencies and managed dependencies.
  A conflict occurs whenever a version is specified for a :dependency which
  is also specified in :managed-dependencies. For each conflict, the
  dependency and managed dependencies are returned as a tuple."
  [project]
  (let [managed-by-key (->> (:managed-dependencies project)
                            (map aether->coordinates)
                            (reduce
                              (fn [acc dep]
                                (assoc acc (unversioned dep) dep))
                              {}))]
    (for [dep (map aether->coordinates (:dependencies project))
          :let [managed (managed-by-key (unversioned dep))]
          :when (and managed (:version dep))]
      [dep managed])))

(defn resolve-targeted-dependency-subtree
  "Returns the minimal dependency subtree that captures all paths to `target`.
  The returned tree has a single nil root node. All leaf nodes of the returned
  tree match `target`."
  [project target]
  (-> (resolve-dependencies project :full-graph)
      (extract-targeted-subtree target)))

(defn- jar-classpath-resources
  "Returns a list of \"classpath resources\" found in the given JAR file.
  \"Classpath resources\" are files in the JAR which should probably be unique
  within the classpath (i.e. they should only appear in one JAR file). This
  detection is heuristic based and may change over time."
  [^File file]
  (with-open [jar (JarFile. file)]
    (->> (enumeration-seq (.entries jar))
         (remove #(.isDirectory %))
         (map #(.getName %))
         ;; Limit to well known extensions. There are otherwise a lot of
         ;; false positives which are difficult to filter out.
         (filter #(let [ext (peek (string/split % #"\."))]
                    (#{"class" "clj" "cljs" "cljc" "edn" "xml" "properties"} ext)))
         ;; Maven metadata files under META-INF frequently conflict, but
         ;; these conflicts are low risk. Some other things under META-INF
         ;; (like Java service loader files) are merged by Leiningen.
         (remove #(string/starts-with? % "META-INF/"))
         ;; module-info.class -> Java 9+ module info (not an actual conflict)
         ;; project.clj -> Leiningen project files (safe to ignore)
         ;; data_readers.clj -> Merged by Leiningen when building an uberjar
         (remove #{"module-info.class"
                   "project.clj"
                   "data_readers.clj"})
         (doall))))

(defn check-classpath-conflicts*
  "Checks for classpath conflicts.
  A classpath conflict occurs when multiple dependencies provide the same
  resource. This detection applies heuristics to limit the number of false
  positives (e.g. LICENSE files appear in multiple JARs). Conflicts are
  returned as a map. Map keys are resource names. Map values are sets of
  dependencies where the resource was found. Only conflicts are included so
  these dependency sets always contain a least 2 entries."
  [project]
  (let [deps (resolve-dependencies project :retrieve)
        dep-files (->> (keys deps)
                       (remove nil?)
                       (reduce
                         (fn [acc dep]
                           (if-let [file (:file (meta dep))]
                             (assoc acc dep file)
                             acc))
                         {}))
        jars (filter #(.endsWith (.getName (val %)) ".jar") dep-files)]
    (->> (mapcat
           (fn [[dep file]]
             (->> (jar-classpath-resources file)
                  (map (fn [entry] [entry dep]))))
           jars)
         (group-by first)
         (reduce-kv
           (fn [acc entry deps]
             (if (> (count deps) 1)
               (assoc acc entry (into [] (map second deps)))
               acc))
           {}))))

(defn- shaded-jar-dependencies
  "Return the coordinates for each dependency shaded within the given JAR."
  [filename]
  (with-open [jar (JarFile. filename)]
    (doall (for [e (enumeration-seq (.entries jar))
                 :when (.endsWith (.getName e) "/pom.properties")]
             (with-open [is (.getInputStream jar e)]
               (let [props (doto (Properties.)
                             (.load is))
                     group-id    (.get props "groupId")
                     artifact-id (.get props "artifactId")
                     version     (.get props "version")]
                 (aether->coordinates
                  [(symbol group-id artifact-id) version])))))))

(defn who-shades*
  "Return a map of dependencies which shade the given target dependency."
  [project target]
  (let [deps (resolve-dependencies project :retrieve)
        dep-files (->> (keys deps)
                       (remove nil?)
                       (remove #(same-dependency? target %))
                       (reduce
                         (fn [acc dep]
                           (let [file (:file (meta dep))]
                             (if (some-> file .getName (.endsWith ".jar"))
                               (assoc acc dep file)
                               acc)))
                         {}))]
    (reduce-kv
     (fn [acc dep jar-path]
       (if-let [version (->> (shaded-jar-dependencies jar-path)
                             (filter #(same-dependency? target %))
                             (first))]
         (assoc acc dep version)
         acc))
     {}
     dep-files)))
