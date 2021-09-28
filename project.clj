(defproject io.github.frenchy64/fully-satisfies "1.6.0"
  :description "A variant of clojure.core/satisfies? that checks all methods are implemented."
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :url "https://github.com/frenchy64/fully-satisfies"
  :dependencies [[org.clojure/clojure "1.10.3"]]
  :java-source-paths ["java"]
  :profiles {:bench
             {:source-paths ["scripts"]
              :dependencies [[com.clojure-goes-fast/clj-async-profiler "0.5.1"]
                             [criterium/criterium "0.4.6"]]
              :jvm-opts ["-Djdk.attach.allowAttachSelf"]}
             :gen-doc
             {:jvm-opts ["--add-opens" "java.base/java.lang=ALL-UNNAMED"]}
             :dev {:dependencies [[org.clojure/test.check "1.1.0"]
                                  [com.gfredericks/test.chuck "0.2.12"
                                   :exclusions [org.clojure/clojurescript
                                                com.andrewmcveigh/cljs-time]]]}}
  :deploy-repositories [["snapshot" {:url "https://clojars.org/repo"
                                     :username :env/clojars_user
                                     :password  :env/clojars_token
                                     :sign-releases false}]
                        ["release" {:url "https://clojars.org/repo"
                                    :username :env/clojars_user
                                    :password  :env/clojars_token
                                    :sign-releases false}]]
  :aliases {"bench" ["with-profile" "+bench" "run" "-m" "benchmark/bench"]}
  :release-tasks [["clean"]
                  ["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag" "--no-sign"]
                  ["shell" "./scripts/regen-latest-version-info.sh"]
                  ["shell" "./scripts/regen-selmer.sh"]
                  ["shell" "./scripts/deploy-doc.sh"]
                  ["deploy" "release"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]]
  :codox {:source-uri "https://github.com/frenchy64/fully-satisfies/blob/{git-commit}/{filepath}#L{line}"}
  :plugins [[lein-codox "0.10.7"]
            [lein-shell "0.5.0"]
            [lein-pprint "1.3.2"]]
  :repl-options {:init-ns io.github.frenchy64.fully-satisfies})
