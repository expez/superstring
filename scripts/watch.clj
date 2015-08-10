(require '[cljs.build.api :as b]
         '[clojure.java.shell :refer [sh]])

(b/watch
 (b/inputs "test" "src")
 {:main 'superstring.core-test
  :target :nodejs
  :output-to "out/tests.js"
  :output-dir "out"
  :watch-fn (fn []
              (let [{:keys [out err] :as res} (sh "node" "out/tests.js")]
                (if (seq out)
                  (println out)
                  (println err))))
  :verbose true})
