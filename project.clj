(defproject scopula "0.1.2"
  :description "Handle a scope convention mechanism to enhance control access"
  :url "https://github.com/threatgrid/scopula"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :lein-release {:deploy-via :clojars
                 :scm :git}
  :repl-options {:init-ns scopula.core})
