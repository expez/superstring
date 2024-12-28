(defproject superstring "3.2.0"
  :description "String manipulation library for clojure and clojurescript"
  :url "http://github.com/expez/superstring"
  :license {:name "Eclipse Public License 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :year 2023
            :key "epl-1.0"}
  :plugins [[codox "0.8.11"]]
  :codox {:src-dir-uri "http://github.com/expez/superstring/blob/master/"
          :src-linenum-anchor-prefix "L"}
  :profiles {:dev {:dependencies [[org.clojure/test.check "1.1.1"]
                                  [org.clojure/clojure "1.11.1"]
                                  [org.clojure/clojurescript"1.11.132" ]
                                  [cider/piggieback "0.5.3"]]
                   :repl-options {:init-ns superstring.core
                                  :nrepl-middleware [cider.piggieback/wrap-cljs-repl]}}})
