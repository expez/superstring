(ns superstring.test-helpers)

(defmacro defexamples [name & examples]
  (if (boolean (:ns &env)) ; test for cljs
    `(cljs.test/deftest ~name
       (cljs.test/are [actual expected] (= actual expected)
         ~@examples))
    `(clojure.test/deftest ~name
       (clojure.test/are [actual expected] (= actual expected)
         ~@examples))))
