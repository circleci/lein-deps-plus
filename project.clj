(defproject com.circleci/deps-plus "0.1.0-SNAPSHOT"
  :pedantic? :abort

  :managed-dependencies [[org.clojure/clojure "1.10.1"]]

  :profiles {:dev {:dependencies [[org.clojure/clojure]
                                  [lambdaisland/kaocha "1.91.1392"]
                                  [lambdaisland/kaocha-cloverage "1.1.89"]
                                  [lambdaisland/kaocha-junit-xml "1.17.101"]]}
             :provided {:dependencies [[org.clojure/clojure]
                                       [com.google.guava/guava "32.0.1-jre"]
                                       [org.jsoup/jsoup "1.15.3"]
                                       [leiningen/leiningen "2.11.2" :exclusions [nrepl org.slf4j/slf4j-api]]]}}

  :aliases {"test"    ["run" "-m" "kaocha.runner"]
            "test-ci" ["test"
                       "--plugin" "cloverage"
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
