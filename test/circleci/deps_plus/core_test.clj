(ns circleci.deps-plus.core-test
  (:require [clojure.test :refer (deftest testing is use-fixtures)])
  (:require [circleci.deps-plus.core :as deps]
            [leiningen.core.project :as lein-project]
            [clojure.string :as str])
  (:import (org.apache.maven.artifact.versioning ComparableVersion)
           (java.io StringWriter)))

;; Needed when not running with :eval-in :leiningen (which would do this for us)
(use-fixtures :once
  (fn [f]
    (lein-project/ensure-dynamic-classloader)
    (f)))

(deftest check-families-tests
  (testing "check-families*"
    (let [project (lein-project/read "test-projects/family-mismatch/project.clj")
          problems (into [] (deps/check-families* project))
          [parent child] (first problems)]
      (is (= 1 (count problems)))
      (is (= "grpc-core" (:artifact-id parent)))
      (is (= "grpc-context" (:artifact-id child)))
      (is (= "1.20.0" (str (:version parent))))
      (is (= "1.19.0" (str (:version child)))))))

(deftest check-downgraded-tests
  (testing "check-downgraded*"
    (let [project (lein-project/read "test-projects/downgrade/project.clj")
          problems (into [] (deps/check-downgraded* project))
          [dep max-version] (first problems)]
      (is (= 1 (count problems)))
      (is (= "com.google.code.gson:gson:jar:2.6:compile" (str dep)))
      (is (= "2.7" (str max-version))))))

(deftest parse-coordinates
  (testing "minimal coordinates"
    (let [coord (deps/parse-coordinates "io.grpc:grpc-core")]
      (is (= "io.grpc" (:group-id coord)))
      (is (= "grpc-core" (:artifact-id coord)))
      (is (= "jar" (:extension coord)))
      (is (= "" (:classifier coord)))
      (is (nil? (:version coord)))
      (is (nil? (:scope (meta coord))))))

  (testing "minimal coordinates with extension"
    (let [coord (deps/parse-coordinates "io.grpc:grpc-core:tar.gz")]
      (is (= "io.grpc" (:group-id coord)))
      (is (= "grpc-core" (:artifact-id coord)))
      (is (= "tar.gz" (:extension coord)))
      (is (= "" (:classifier coord)))
      (is (nil? (:version coord)))
      (is (nil? (:scope (meta coord))))))

  (testing "basic coordinates"
    (let [coord (deps/parse-coordinates "io.grpc:grpc-core:jar:1.20.0")]
      (is (= "io.grpc" (:group-id coord)))
      (is (= "grpc-core" (:artifact-id coord)))
      (is (= "jar" (:extension coord)))
      (is (= "" (:classifier coord)))
      (is (= (ComparableVersion. "1.20.0") (:version coord)))
      (is (nil? (:scope (meta coord))))))

  (testing "with scope"
    (let [coord (deps/parse-coordinates "io.grpc:grpc-core:jar:1.20.0:provided")]
      (is (= "io.grpc" (:group-id coord)))
      (is (= "grpc-core" (:artifact-id coord)))
      (is (= "jar" (:extension coord)))
      (is (= "" (:classifier coord)))
      (is (= (ComparableVersion. "1.20.0") (:version coord)))
      (is (= "provided" (:scope (meta coord))))))

  (testing "with classifier"
    (let [coord (deps/parse-coordinates "io.grpc:grpc-core:jar:sources:1.20.0")]
      (is (= "io.grpc" (:group-id coord)))
      (is (= "grpc-core" (:artifact-id coord)))
      (is (= "jar" (:extension coord)))
      (is (= "sources" (:classifier coord)))
      (is (= (ComparableVersion. "1.20.0") (:version coord)))
      (is (nil? (:scope (meta coord))))))

  (testing "with scope and classifier"
    (let [coord (deps/parse-coordinates "io.grpc:grpc-core:jar:sources:1.20.0:provided")]
      (is (= "io.grpc" (:group-id coord)))
      (is (= "grpc-core" (:artifact-id coord)))
      (is (= "jar" (:extension coord)))
      (is (= "sources" (:classifier coord)))
      (is (= (ComparableVersion. "1.20.0") (:version coord)))
      (is (= "provided" (:scope (meta coord)))))))

(deftest write-dependency-list
  (let [project (lein-project/read "test-projects/dependency-list/project.clj" [])
        buffer (StringWriter.)
        _ (deps/write-dependency-list project buffer)
        lines (str/split-lines (str buffer))]
    (is (= "com.google.code.gson:gson:jar:2.7:compile" (first lines)))
    (is (= "io.grpc:grpc-context:jar:1.20.0:compile" (second lines)))))

(deftest diff-dependency-list
  (let [project (lein-project/read "test-projects/dependency-diff/project.clj" [])
        list (slurp "test-projects/dependency-diff/list")
        buffer (StringWriter.)
        _ (deps/diff-dependency-list project list buffer)
        lines (str/split-lines (str buffer))]
    (is (= [";; Added"
            "fixture:added:jar:1.0:compile"
            "fixture:changed-classifier:jar:1.0:compile"
            "fixture:changed-extension:jar:1.0:compile"
            ""
            ";; Removed"
            "fixture:changed-classifier:jar:sources:1.0:compile"
            "fixture:changed-extension:tar.gz:1.0:compile"
            "fixture:removed:jar:1.0:compile"
            ""
            ";; Upgraded"
            "fixture:upgraded:jar:2.0:compile (1.0 -> 2.0)"
            ""
            ";; Downgraded"
            "fixture:downgraded:jar:0.9:compile (1.0 -> 0.9)"
            ""
            ";; Rescoped"
            "fixture:changed-scope:jar:1.0:compile (test -> compile)"]
           lines)))
  (testing "empty sections aren't included"
    (let [project (lein-project/read "test-projects/dependency-diff/project.clj" [])
          buffer (StringWriter.)
          _ (deps/diff-dependency-list project "" buffer)
          out (str buffer)]
      (is (= [";; Added"]
             (filter (fn [line] (str/starts-with? line ";; "))
                     (str/split-lines out)))))))

(deftest find-pedantic-conflicts-tests
  (testing "version conflict"
    (let [project (lein-project/read "test-projects/pedantic-version-conflict/project.clj" [])
          conflicts (deps/find-pedantic-conflicts project)]
      (is (= 1 (count conflicts)))
      (is (= "io.grpc:grpc-core:jar:1.19.0:compile" (str (first (first conflicts)))))))

  (testing "version range"
    (let [project (lein-project/read "test-projects/pedantic-range/project.clj" [])
          conflicts (deps/find-pedantic-conflicts project)]
      (is (= 1 (count conflicts)))
      (is (= "io.grpc:grpc-core:jar:1.19.0:compile" (str (first (first conflicts))))))))

(deftest parse-dependency-list-test
  (testing "empty list"
    (let [deps (#'deps/parse-dependency-list "")]
      (is (empty? deps))))

  (testing "one dependency"
    (let [deps (#'deps/parse-dependency-list "io.grpc:grpc-core:jar:1.19.0:compile")
          {:keys [group-id artifact-id extension classifier version]} (first deps)]
      (is (= 1 (count deps)))
      (is (= "io.grpc" group-id))
      (is (= "grpc-core" artifact-id))
      (is (= "jar" extension))
      (is (= "" classifier))
      (is (= (ComparableVersion. "1.19.0") version))
      (is (= "compile" (:scope (meta (first deps)))))))

  (testing "multiple dependencies"
    (let [deps (#'deps/parse-dependency-list (str "io.grpc:grpc-core:jar:1.19.0:compile\n"
                                             "io.grpc:grpc-api:jar:1.21.0:compile\n"))]
      (is (= 2 (count deps)))
      (let [{:keys [group-id artifact-id extension classifier version]} (first deps)]
        (is (= "io.grpc" group-id))
        (is (= "grpc-core" artifact-id))
        (is (= "jar" extension))
        (is (= "" classifier))
        (is (= (ComparableVersion. "1.19.0") version))
        (is (= "compile" (:scope (meta (first deps))))))
      (let [{:keys [group-id artifact-id extension classifier version]} (second deps)]
        (is (= "io.grpc" group-id))
        (is (= "grpc-api" artifact-id))
        (is (= "jar" extension))
        (is (= "" classifier))
        (is (= (ComparableVersion. "1.21.0") version))
        (is (= "compile" (:scope (meta (first deps)))))))))

(deftest check-management-conflicts-tests
  (testing "management conflict"
    (let [project (lein-project/read "test-projects/management-conflict/project.clj" [])
          conflicts (deps/check-management-conflicts* project)]
      (is (= 1 (count conflicts)))
      (let [[dep managed] (first conflicts)]
        (is (= "io.grpc:grpc-api:jar:1.22.0" (str dep)))
        (is (= "io.grpc:grpc-api:jar:1.21.0" (str managed)))))))

(deftest resolve-dependencies
  (testing "standard resolution includes ranges"
    (let [project (lein-project/read "test-projects/pedantic-range/project.clj" [])
          resolved (deps/resolve-dependencies project)
          grpc-cores (->> (vals resolved)
                          (mapcat identity)
                          (filter #(= "grpc-core" (:artifact-id %))))]
      (is (< 0 (count grpc-cores)))
      ;; Every copy should be identical and should therefore have a version range.
      (is (every? (comp :version-range meta) grpc-cores))
      (is (= "[1.19.0,1.19.0]" (str (:version-range (meta (first grpc-cores))))))))

  (testing "full-graph resolution includes ranges"
      (let [project (lein-project/read "test-projects/pedantic-range/project.clj" [])
            resolved (deps/resolve-dependencies project :full-graph)
            grpc-cores (->> (vals resolved)
                            (mapcat identity)
                            (filter #(= "grpc-core" (:artifact-id %))))]
        ;; We should have copies both with and without version ranges
        (is (< 0 (count (filter (comp :version-range meta) grpc-cores))))
        (is (< 0 (count (remove (comp :version-range meta) grpc-cores))))))

  (testing "standard resolution includes premanaged version"
    (let [project (lein-project/read "test-projects/downgrade/project.clj" [])
          resolved (deps/resolve-dependencies project)
          gson (->> (keys resolved)
                    (filter #(= "gson" (:artifact-id %)))
                    (first))]
      (is (some? gson))
      (is (= "2.6" (str (:version gson))))
      (is (= "2.7" (str (:premanaged-version (meta gson)))))))

  (testing "full-graph resolution includes premanaged version"
    (let [project (lein-project/read "test-projects/downgrade/project.clj" [])
          resolved (deps/resolve-dependencies project :full-graph)
          gson (->> (keys resolved)
                    (filter #(= "gson" (:artifact-id %)))
                    (first))]
      (is (some? gson))
      (is (= "2.6" (str (:version gson))))
      (is (= "2.7" (str (:premanaged-version (meta gson))))))))

(deftest check-classpath-conflicts-test
  (let [project (lein-project/read "test-projects/classpath-conflict/project.clj" [])
        conflicts (deps/check-classpath-conflicts* project)
        conflicting-deps (get conflicts "com/google/common/base/Preconditions.class")]
    (is (= 2 (count conflicting-deps)))
    (is (= #{"guava" "guava-jdk5"} (into #{} (map :artifact-id conflicting-deps))))))

(deftest who-shades
  (testing "dependency only appearing as shaded"
    (let [project (lein-project/read "test-projects/shaded-dependencies/project.clj" [])
          target  (deps/parse-coordinates "commons-codec:commons-codec")
          result  (deps/who-shades* project target)]
      (is (= 1 (count result)))
      (let [[shader shadee] (first result)]
        (is (= "io.honeycomb.libhoney:libhoney-java:jar:1.3.1:compile" (str shader)))
        (is (= "commons-codec:commons-codec:jar:1.9" (str shadee))))))

  ;; This case validates that who-shades does not return self
  ;; matches (e.g. netty-common does not shade itself).
  (testing "dependency appearing as both shaded and standalone"
    (let [project (lein-project/read "test-projects/shaded-dependencies/project.clj" [])
          target  (deps/parse-coordinates "io.netty:netty-common")
          result  (deps/who-shades* project target)]
      (is (= 1 (count result)))
      (let [[shader shadee] (first result)]
        (is (= "io.grpc:grpc-netty-shaded:jar:1.20.0:compile" (str shader)))
        (is (= "io.netty:netty-common:jar:4.1.34.Final" (str shadee))))))

  ;; This behavior could be changed, but at least at the moment the version
  ;; number in the target is ignored.
  (testing "dependency only appearing as shaded"
    (let [project (lein-project/read "test-projects/shaded-dependencies/project.clj" [])
          target  (deps/parse-coordinates "commons-codec:commons-codec:jar:1.2.3")
          result  (deps/who-shades* project target)]
      (is (= 1 (count result)))
      (let [[shader shadee] (first result)]
        (is (= "io.honeycomb.libhoney:libhoney-java:jar:1.3.1:compile" (str shader)))
        (is (= "commons-codec:commons-codec:jar:1.9" (str shadee))))))

  (testing "no matches"
    (let [project (lein-project/read "test-projects/shaded-dependencies/project.clj" [])
          target  (deps/parse-coordinates "super-fake:so-fake:jar:what-big-versions")
          result  (deps/who-shades* project target)]
      (is (empty? result)))))

(deftest check-forbidden-dependencies
  (testing "When there are no forbidden dependencies"
    (let [project (lein-project/read "test-projects/shaded-dependencies/project.clj")]
      (is (nil? (deps/check-forbidden-dependencies project)))))
  (testing "When there are forbidden dependencies"
    (let [project (lein-project/read "test-projects/forbidden-dependencies/project.clj")]
      (is (= #{'com.taoensso/nippy 'io.netty/netty-common}
             (deps/check-forbidden-dependencies project))))))
