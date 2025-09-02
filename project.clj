(defproject threatgrid/scopula "0.3.3"
  :description "Offers a scope convention mechanism to enhance control access."
  :url "https://github.com/threatgrid/scopula"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"
            :distribution :repo}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/clojurescript "1.11.132"]]
  :deploy-repositories [["releases"  {:url "https://clojars.org/repo" :creds :gpg}]
                        ["snapshots" {:url "https://clojars.org/repo" :creds :gpg}]]
  :repl-options {:init-ns scopula.core }
  :plugins [[lein-cljsbuild "1.1.8"]]
  :profiles {:benchmarks {:dependencies [[criterium "0.4.6"]
                                         [com.clojure-goes-fast/clj-memory-meter "0.4.0"]]
                          :source-paths ["benchmark"]
                          :jvm-opts ["-Djdk.attach.allowAttachSelf"
                                     "-XX:+EnableDynamicAgentLoading"
                                     ]}}
  :cljsbuild {:source-paths ["src"]
              :compiler {:optimizations :whitespace
                         :pretty-print false}})
