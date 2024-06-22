(defproject cljsearch "0.1.0-SNAPSHOT"
  :description "cljsearch - the clojure version of the xsearch command-line-based search utility"
  :url "https://github.com/clarkcb/xsearch"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                  [org.clojure/clojure "1.11.2"]
                  [org.clojure/data.json "2.5.0"]
                  [clojure.java-time "1.4.2"]
                  [cljfind "0.1.0-SNAPSHOT"]
                ]
  :main ^:skip-aot cljsearch.cljsearch
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
