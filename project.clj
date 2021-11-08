(defproject com.circleci/deps-plus "0.1.0-SNAPSHOT"
  :plugins [[jonase/eastwood "0.3.6" :exclusions [org.clojure/clojure]]]

  :pedantic? :abort

  :managed-dependencies [[org.clojure/clojure "1.10.1"]]

  :profiles {:dev {:dependencies [[org.clojure/clojure]
                                  [lambdaisland/kaocha "0.0-601"]
                                  [lambdaisland/kaocha-cloverage "0.0-41"]
                                  [lambdaisland/kaocha-junit-xml "0.0-70"]]}
             :provided {:dependencies [[org.clojure/clojure]
                                       [com.google.guava/guava "30.0-jre"]
                                       [org.jsoup/jsoup "1.14.2"]
                                       [leiningen/leiningen "2.9.1"]]}}

  :aliases {"test"    ["run" "-m" "kaocha.runner"]
            "test-ci" ["test"
                       "--plugin" "cloverage"
                       "--codecov" "--no-cov-html" "--no-cov-summary"
                       "--plugin" "kaocha.plugin/profiling"
                       "--plugin" "kaocha.plugin/junit-xml"
                       "--junit-xml-file" "target/test-results/results.xml"]}
  :repositories [["releases" {:url "https://clojars.org/repo"
                              :username :env/clojars_username
                              :password :env/clojars_token
                              :sign-releases false}]
                 ["snapshots" {:url "https://clojars.org/repo"
                               :username :env/clojars_username
                               :password :env/clojars_token
                               :sign-releases false}]])
