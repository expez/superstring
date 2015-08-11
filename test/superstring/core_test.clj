(ns superstring.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check
             [clojure-test :refer [defspec]]
             [generators :as gen]
             [properties :as prop]]
            [superstring.core :as str]))

(defmacro defexamples [name & examples]
  `(deftest ~name
     (are [actual expected] (= actual expected)
       ~@examples)))

(defspec appending-separator-and-chomping-does-not-alter-length 100
  (prop/for-all [s gen/string
                 sep gen/string]
    (let [res (str/chomp s)]
      (= (count (str/chomp (str s sep) sep))
         (count s)))))

(defspec chomping-string-not-ending-in-seperator-does-not-alter-length 100
  (prop/for-all
      [[sep s] (gen/bind (gen/not-empty gen/string)
                         (fn [sep]
                           (gen/tuple (gen/return sep)
                                      (gen/such-that #(not (.endsWith % sep))
                                                     gen/string))))]
    (= (count (str/chomp s sep)) (count s))))

(defspec chomp-removes-newline 100
  (prop/for-all [s gen/string]
    (= (.length (str/chomp (str s "\n")))) (.length s)))

(defspec chomp-removes-carriage-return 100
  (prop/for-all [s gen/string]
    (= (.length (str/chomp (str s "\r"))) (.length s))))

(defspec chomp-removes-carriage-return-line-feed 100
  (prop/for-all [s gen/string]
    (= (.length (str/chomp (str s "\r\n"))) (.length s))))

(defexamples chomp-examples
  (str/chomp "" "") ""
  (str/chomp "foo" "foo") ""
  (str/chomp "foobar" "bar") "foo"
  (str/chomp "foo\n") "foo"
  (str/chomp "foo\r") "foo"
  (str/chomp "foo\r\n") "foo"
  (str/chomp "foo\n\r") "foo\n")

(defspec ends-with?-acts-like-endsWith 100
  (prop/for-all [s gen/string
                 suffix gen/string]
    (= (.endsWith (str s suffix) suffix)
       (if (str/ends-with? (str s suffix) suffix) true false))))

(defspec ends-with?-can-ignore-case 100
  (prop/for-all [s gen/string
                 suffix gen/string]
    (= (.endsWith (str s suffix) suffix)
       (if (str/ends-with? (str s suffix) suffix) true false))))

(def string-without-german-b (gen/such-that (fn [s] (not (str/contains? s "ß")))
                                            gen/string))

(defspec ends-with?-can-ignore-case 100
  (prop/for-all [s gen/string
                 suffix string-without-german-b]
    (str/ends-with? (str s suffix ) (str/swap-case suffix) :ignore-case)))

(defexamples ends-with?
  (str/ends-with? "foobar" "foo") nil
  (str/ends-with? "ß" "ss" :ignore-case) nil
  (str/ends-with? "ß" "SS" :ignore-case) nil
  (str/ends-with? "aß" "ß" :ignore-case) "aß")

(defexamples starts-with?
  (str/starts-with? "foo" "foobar") nil
  (str/starts-with? "ß" "ss" :ignore-case) nil
  (str/starts-with? "ß" "SS" :ignore-case) nil
  (str/starts-with? "ßa" "ß" :ignore-case) "ßa")

(defspec starts-with?-acts-like-startsWith 100
  (prop/for-all [s gen/string
                 prefix gen/string]
    (= (.startsWith (str prefix s) prefix)
       (if (str/starts-with? (str prefix s) prefix) true false))))

(defspec starts-with?-can-ignore-case 100
  (prop/for-all [s gen/string
                 prefix string-without-german-b]
    (str/starts-with? (str prefix s) (str/swap-case prefix) :ignore-case)))

(defspec chop-reduces-length-by-1-without-cr 100
  (prop/for-all [s (gen/such-that #(not (str/ends-with? % "\r\n")) gen/string)]
    (= (.length (str/chop s)) (max 0 (dec (.length s))))))

(defspec chop-gets-rid-of-both-chars-in-crlf 100
  (prop/for-all [s gen/string]
    (let [s (str s "\r\n")]
      (= (.length (str/chop s)) (max 0 (- (.length s) 2))))))

(deftest chopping-the-empty-string-is-a-no-op []
  (is (= (str/chop "") "")))

(defn- case-to-int [c]
  ;; some chars, like \ß, are lower-case but upcase to themselves
  (let [exists-in-upper-and-lower (fn [c]
                                    (when (or (Character/isUpperCase c)
                                              (Character/isLowerCase c))
                                      (if (Character/isUpperCase c)
                                        (not= c (Character/toUpperCase c))
                                        (not= c (Character/toLowerCase c)))))]
    (if (exists-in-upper-and-lower c)
      (if (Character/isUpperCase c) -1 1)
      0)))

(defspec swap-case-changes-case 100
  (prop/for-all [s string-without-german-b]
    (let [count-uppers (partial reduce (fn [acc c] (if (str/upper-case? (str c))
                                                     (inc acc) 0)) 0)
          count-lowers (partial reduce (fn [acc c] (if (str/lower-case? (str c))
                                                     (inc acc) 0)) 0)]
      (and (= (count-uppers s) (count-lowers (str/swap-case s)))
           (= (count-lowers s) (count-uppers (str/swap-case s)))))))

(defexamples swap-case
  (str/swap-case "fOO" ) "Foo"
  (str/swap-case "FOO") "foo"
  (str/swap-case "Ååberg") "åÅBERG"
  (str/swap-case "ÆæÅ.") "æÆå."
  (str/swap-case "ß") "SS")

(defspec slice-without-end-has-length-1 100
  (prop/for-all [[s i] (gen/bind (gen/not-empty gen/string)
                                 (fn [s]
                                   (gen/tuple (gen/return s)
                                              (gen/such-that #(< % (.length s))
                                                             gen/int 100))))]
    (= (.length (str/slice s 0)) 1)))

(defspec slice-with-length-outside-string 100
  ;; When beg + end falls outside the string we return the rest of the
  ;; string starting from beg
  (prop/for-all [s (gen/not-empty gen/string)]
    (let [beg (rand-int (.length s))]
      (= (str/slice s beg (+ (.length s) beg)) (.substring s beg)))))

(defspec slices-with-index-outside-str-is-nil 100
  (prop/for-all [[s index] (gen/bind (gen/not-empty gen/string)
                                     (fn [s]
                                       (gen/tuple
                                        (gen/return s)
                                        (gen/such-that #(> (Math/abs %) (.length s))
                                                       gen/int 100))))]
    (let [len (inc (rand-int (dec (.length s))))]
      (nil? (str/slice s index len)))))

(defspec slices-with-negative-lengths-are-nil 100
  (prop/for-all [len (gen/such-that #(not (zero? %)) gen/neg-int)
                 s gen/string
                 index gen/int]
    (nil? (str/slice s index len))))

(defexamples slice
  (str/slice "" 0) nil
  (str/slice "" 1) nil
  (str/slice "12" 1 -1) nil
  (str/slice "1" 0) "1"
  (str/slice "12" 1) "2"
  (str/slice "12" 0 2) "12"
  (str/slice "0123456" 3 100) "3456"
  (str/slice "0123456" -1) "6"
  (str/slice "0123456" -3 2) "45"
  (str/slice "0123456" -3 100) "456")

(defspec right-pad-results-in-strings-with-new-width 100
  (prop/for-all
      [vals
       (gen/bind gen/string
                 (fn [s]
                   (gen/tuple (gen/return s)
                              (gen/such-that #(> % (.length s)) gen/pos-int 100))))]
    (let [s (first vals)
          width (second vals)]
      (= (.length (str/pad-right s width)) width))))

(defexamples right-pad
  (str/pad-right "" 0) ""
  (str/pad-right "" 1) " "
  (str/pad-right "foo" 4) "foo "
  (str/pad-right "foo" 5) "foo  "
  (str/pad-right "foo" 5 ".!") "foo.!"
  (str/pad-right "foo" 6 ".!") "foo.!.")

(defexamples left-pad
  (str/pad-left "" 0) ""
  (str/pad-left "" 1) " "
  (str/pad-left "foo" 4) " foo"
  (str/pad-left "foo" 5) "  foo"
  (str/pad-left "foo" 5 ".!") ".!foo"
  (str/pad-left "foo" 6 ".!") ".!.foo")

(defspec left-pad-results-in-strings-with-new-width 100
  (prop/for-all
      [[s width]
       (gen/bind gen/string
                 (fn [s]
                   (gen/tuple (gen/return s)
                              (gen/such-that #(> % (.length s)) gen/pos-int 100))))]
    (= (.length (str/pad-left s width)) width)))

(defexamples center
  (str/center "" 0) ""
  (str/center "" 1) " "
  (str/center "foo" 4) "foo "
  (str/center "foo" 5) " foo "
  (str/center "foo" 5 ".!") "foo.!"
  (str/center "foo" 4 ".!") "foo."
  (str/center "foo" 6 ".!") ".!foo."
  (str/center "foo" 8 ".!") ".!foo.!.")

(defspec center-results-in-strings-with-new-width 100
  (prop/for-all
      [[s width]
       (gen/bind gen/string
                 (fn [s]
                   (gen/tuple (gen/return s)
                              (gen/such-that #(> % (.length s)) gen/pos-int 100))))]
    (= (.length (str/center s width)) width)))

(defexamples chop-suffix
  (str/chop-suffix "" "foo") ""
  (str/chop-suffix "foo" "foo") ""
  (str/chop-suffix "foobar" "bar") "foo"
  (str/chop-suffix "foo " " ") "foo"
  (str/chop-suffix "foo" "bar") "foo"
  (str/chop-suffix "foo" "FOO") "foo"
  (str/chop-suffix "foo" "O") "foo"
  (str/chop-suffix "fooÅ" "å") "fooÅ"
  (str/chop-suffix "foo" "FOO" :ignore-case) ""
  (str/chop-suffix "foo" "O" :ignore-case) "fo"
  (str/chop-suffix "fooÅ" "å" :ignore-case) "foo")

(defexamples chop-prefix
  (str/chop-prefix "" "foo") ""
  (str/chop-prefix "foo" "foo") ""
  (str/chop-prefix "foobar" "foo")   "bar"
  (str/chop-prefix " foo" " ") "foo"
  (str/chop-prefix "foo" "bar") "foo"
  (str/chop-prefix "foo" "FOO") "foo"
  (str/chop-prefix "foo" "F") "foo"
  (str/chop-prefix "Åfoo" "å") "Åfoo"
  (str/chop-prefix "foo" "FOO" :ignore-case) ""
  (str/chop-prefix "foo" "F" :ignore-case) "oo"
  (str/chop-prefix "Åfoo" "Å" :ignore-case) "foo")

(defexamples contains-test?
  (str/contains? "" "") ""
  (str/contains? "1" "1") "1"
  (str/contains? "foo" "fo") "foo"
  (str/contains? "foobar" "qux") nil
  (str/contains? "foobar" "BAR") nil
  (str/contains? "foobar" "BAR" :ignore-case) "foobar"
  (str/contains? "Albert Åberg" "åberg" :ignore-case) "Albert Åberg")

(defspec contains?-finds-generated-strings 100
  (prop/for-all [before gen/string
                 needle (gen/not-empty gen/string)
                 after gen/string]
    (str/contains? (str before needle after) needle)))

(defn- randomly-swapcase
  [s]
  (let [swapcase (fn [c]
                   (if (Character/isUpperCase c)
                     (Character/toLowerCase c)
                     (if (Character/isLowerCase c)
                       (Character/toUpperCase c)
                       c)))]
    (apply str (map #(if (> (rand-int 3) 1) (swapcase %) %) s))))

(defspec contains?-can-ignore-case 100
  (prop/for-all [before gen/string
                 needle (gen/fmap randomly-swapcase (gen/not-empty gen/string))
                 after gen/string]
    (str/contains? (str before needle after) needle :ignore-case)))

(defexamples contains-test?
  (str/contains? "" "") ""
  (str/contains? "1" "1") "1"
  (str/contains? "foo" "fo") "foo"
  (str/contains? "foobar" "qux") nil
  (str/contains? "foobar" "BAR") nil
  (str/contains? "foobar" "BAR" :ignore-case) "foobar"
  (str/contains? "fooß" "ss" :ignore-case) nil
  (str/contains? "fooß" "SS" :ignore-case) nil
  (str/contains? "ß" "SS" :ignore-case) nil
  (str/contains? "Albert Åberg" "åberg" :ignore-case) "Albert Åberg")

(defspec contains-all-finds-generated-strings? 100
  (prop/for-all [s1 gen/string
                 s2 gen/string
                 s3 gen/string]
    (str/contains-all? (str s1 s2 s3) [s1 s2 s3])))

(defexamples contains-any?
  (str/contains-any? "foobar" ["foo"]) "foobar"
  (str/contains-any? "foobar" ["qux"]) nil
  (str/contains-any? "foobar" ["qux" "bar"]) "foobar"
  (str/contains-any? "ß" ["ss" "SS"] :ignore-case) nil
  (str/contains-any? "foobar" ["BAR"] :ignore-case) "foobar")

(defspec contains-any-finds-a-needle 100
  (prop/for-all [before (gen/not-empty gen/string)
                 needle (gen/not-empty gen/string)
                 after (gen/not-empty gen/string)]
    (str/contains-any? (str before needle after) [needle])))

(defspec contains-any-can-ignore-case 100
  (prop/for-all [before (gen/not-empty gen/string)
                 needle (gen/not-empty gen/string)
                 after (gen/not-empty gen/string)]
    (-> before
        (str (randomly-swapcase needle) after)
        (str/contains-any? [needle] :ignore-case)
        is)))

(defspec truncated-strings-have-right-length 100
  (prop/for-all [[s len] (gen/bind (gen/such-that #(> (.length %) 3) gen/string 100)
                                   (fn [s]
                                     (gen/tuple
                                      (gen/return s)
                                      (gen/choose 3 (max 4 (- (.length s) 3)) ))))]
    (= (.length (str/truncate s len)) (min (.length s) len))))

(defexamples truncate
  (str/truncate "" 3) ""
  (str/truncate "123456" 3) "..."
  (str/truncate "123456" 6) "123..."
  (str/truncate "1" 3) "1"
  (str/truncate "12" 3) "12"
  (str/truncate "123" 3) "123")

(defexamples common-prefix
  (str/common-prefix "321" "123") ""
  (str/common-prefix "Åffø123456" "Åfføo8yuidfg") "Åffø"
  (str/common-prefix "123456" "123o8yuidfg") "123"
  (str/common-prefix "Åberg" "åberg") ""
  (str/common-prefix "åberg" "Åberg" :ignore-case) "Åberg"
  (str/common-prefix "Åberg" "åberg" :ignore-case) "åberg")

(defspec common-prefix-finds-common-prefixes 100
  (prop/for-all [prefix (gen/not-empty gen/string)
                 s1 gen/string
                 s2 gen/string]
    (.startsWith (str/common-prefix (str prefix s1) (str prefix s2)) prefix)))


(defexamples common-suffix
  (str/common-suffix "321" "123") ""
  (str/common-suffix "123456Åffø" "o8yuidfgÅffø") "Åffø"
  (str/common-suffix "456123" "o8yuidfg123") "123"
  (str/common-suffix "åberG" "åberg") ""
  (str/common-suffix "åberg" "åberG" :ignore-case) "åberG"
  (str/common-suffix "Åberg" "åberg" :ignore-case) "åberg")

(defspec common-suffix-finds-common-suffixes 100
  (prop/for-all [suffix (gen/not-empty gen/string)
                 s1 gen/string
                 s2 gen/string]
    (.endsWith (str/common-suffix (str s1 suffix) (str s2 suffix)) suffix)))

(defn upper-if-upper-exists?
  "Is c uppercase for those characters which have an upper case version?"
  [^Character c]
  (or (Character/isUpperCase c) (= c (Character/toUpperCase c))))

(defn lower-if-lower-exists?
  "Is c lower for those characters which have an lower case version?"
  [^Character c]
  (or (Character/isLowerCase c) (= c (Character/toLowerCase c))))

(defspec title-case-starts-with-upper 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (upper-if-upper-exists? (first (str/title-case s)))))

(defspec title-case-rest-is-all-lower-case 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (reduce (fn [acc c] (and acc (lower-if-lower-exists? c))) true
            (rest (str/title-case s)))))

(defspec upper-case?-returns-true-on-all-upper 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (str/upper-case? (str/upper-case s))))

(defspec upper-case?-returns-nil-on-all-lower 100
  (prop/for-all [s (gen/such-that not-empty
                                  (gen/fmap str/join (gen/vector gen/char-alpha)))]
    (not (str/upper-case? (str/lower-case s)))))

(deftest upper-case-test
  (str/upper-case? "UPPER") "UPPER"
  (str/upper-case? "123UPPER!") "123UPPER!")

(defspec lower-case?-returns-true-on-all-lower 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (str/lower-case? (str/lower-case s))))

(defspec lower-case?-returns-nil-on-all-upper 100
  (prop/for-all [s (gen/such-that not-empty
                                  (gen/fmap str/join (gen/vector gen/char-alpha)))]
    (not (str/lower-case? (str/upper-case s)))))

(defn- word []
  (gen/fmap #(if (< (.length %) 10)
               % (.substring % 0 (rand-int (.length %))))
            (gen/not-empty gen/string-alphanumeric)))

(defn- less-than-width-or-unbreakable [line width]
  (or (<= (.length line) width)
      (not (re-find #" " line))))

(defspec wrap-words-have-lines-no-longer-than-max-width 25
  (prop/for-all [words (gen/bind (gen/choose 50 500)
                                 #(gen/return (gen/sample (word) %)))
                 width (gen/choose 8 80)]
    (reduce (fn [acc line]
              (and acc (less-than-width-or-unbreakable line width)))
            true
            (str/split-lines (str/wrap-words (str/join " " words) width)))))

(defexamples lower-case-test
  (str/lower-case? "upper") "upper"
  (str/lower-case? "123upper!") "123upper!")

(defspec lisp-case-is-all-lower 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (reduce (fn [acc c] (and acc (lower-if-lower-exists? c))) true
            (str/lisp-case s))))

(defexamples lisp-case-test
  (str/lisp-case "PascalCase") "pascal-case"
  (str/lisp-case "setID") "set-id"
  (str/lisp-case "HTTPRequest") "http-request"
  (str/lisp-case "snake_case") "snake-case"
  (str/lisp-case "SCREAMING_SNAKE_CASE") "screaming-snake-case")

(defexamples camel-case-test
  (str/camel-case "PascalCase") "pascalCase"
  (str/camel-case "setID") "setId"
  (str/camel-case "HTTPRequest") "httpRequest"
  (str/camel-case "snake_case") "snakeCase"
  (str/camel-case "SCREAMING_SNAKE_CASE") "screamingSnakeCase")

(defexamples pascal-case-test
  (str/pascal-case "PascalCase") "PascalCase"
  (str/pascal-case "setID") "SetId"
  (str/pascal-case "HTTPRequest") "HttpRequest"
  (str/pascal-case "snake_case") "SnakeCase"
  (str/pascal-case "SCREAMING_SNAKE_CASE") "ScreamingSnakeCase")

(defexamples snake-case-test
  (str/snake-case "PascalCase") "pascal_case"
  (str/snake-case "setID") "set_id"
  (str/snake-case "HTTPRequest") "http_request"
  (str/snake-case "snake_case") "snake_case"
  (str/snake-case "SCREAMING_SNAKE_CASE") "screaming_snake_case")

(defexamples screaming-snake-case-test
  (str/screaming-snake-case "PascalCase") "PASCAL_CASE"
  (str/screaming-snake-case "setID") "SET_ID"
  (str/screaming-snake-case "HTTPRequest") "HTTP_REQUEST"
  (str/screaming-snake-case "snake_case") "SNAKE_CASE"
  (str/screaming-snake-case "SCREAMING_SNAKE_CASE") "SCREAMING_SNAKE_CASE")

(defexamples strip-accents-test
  (str/strip-accents "Et ça sera sa moitié") "Et ca sera sa moitie"
  (str/strip-accents "ąàáäâãåæăćčĉęèéëêĝĥìíïîĵľńňòóöőôõøśșšŝťțŭùúüűûñÿýçżźž")
  "aaaaaaaæaccceeeeeghiiiijlnnooooooøssssttuuuuuunyyczzz")

(def string-ascii
  (gen/fmap #(apply str %) (gen/vector gen/char-alpha)))

(defspec ascii?-returns-s-on-ascii-strings 100
  (prop/for-all [s (gen/not-empty gen/string-ascii)]
    (= s (str/ascii? s))))

(defexamples ascii?-test
  (str/ascii? "ascii") "ascii"
  (str/ascii? "Et ça sera sa moitié") nil)

(defspec slug-contains-only-ascii-chars 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (str/ascii? (str/slug s))))

(defspec slug-contains-no-whitespace 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (not (re-find #"\s+" (str/slug s)))))

(defspec slug-is-all-lower 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (str/lower-case? (str/slug s))))

(defspec slug-only-contains-unreserved-characters 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (not (re-matches #"[^A-Za-z0-9_.~-]+" (str/slug s)))))

(defexamples slug-test
  (str/slug "This, That & the Other! Various Outré   Considerations")
  "this-that-the-other-various-outre-considerations")

(def string-alpha
  (gen/fmap #(apply str %) (gen/vector gen/char-alpha)))

(defspec mixed-case-is-true-for-mixed-case-strings 100
  (prop/for-all [s1 (gen/not-empty string-alpha)
                 s2 (gen/not-empty string-alpha)]
    (str/mixed-case? (str (str/upper-case s1) (str/lower-case s2)))))

(defspec mixed-case-is-false-for-upper-case-strings 100
  (prop/for-all [s string-ascii]
    (not (str/mixed-case? (str/upper-case s)))))

(defexamples mixed-case?-test
  (str/mixed-case? "1aB") "1aB"
  (str/mixed-case? "1B") nil
  (str/mixed-case? "1234") nil
  (str/mixed-case? "FooBar") "FooBar"
  (str/mixed-case? "foo") nil)

(def whitespace
  (gen/fmap #(apply str %)
            (gen/not-empty (gen/vector
                            (gen/elements [\tab \newline \return \space])))))

(defspec collapse-whitespace-collapses-whitespace 100
  (prop/for-all [s (gen/not-empty gen/string-alphanumeric)
                 ws1 (gen/not-empty whitespace)
                 ws2 (gen/not-empty whitespace)]
    (= (+ (.length s) 2)
       (.length (str/collapse-whitespace (str ws1 s ws2))))))

(defexamples collapse-whitespace-test
  (str/collapse-whitespace "foo

bar    	baz") "foo bar baz"
(str/collapse-whitespace " foo bar baz ") " foo bar baz ")

(defspec levenshtein-distance-is-at-least-difference-between-string-lenghts 100
  (prop/for-all [s1 (gen/not-empty gen/string)
                 s2 (gen/not-empty gen/string)]
    (>= (str/distance s1 s2)
        (- (max (.length s1) (.length s2))
           (min (.length s1) (.length s2))))))

(defspec levenshtein-distance-is-at-most-length-of-longest-string 100
  (prop/for-all [s1 (gen/not-empty gen/string)
                 s2 (gen/not-empty gen/string)]
    (<= (str/distance s1 s2)
        (max (.length s1) (.length s2)))))

(defspec levenshtein-distance-is-zero-for-equal-strings 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (= 0 (str/distance s s))))

(defspec levenshtein-distance-is-zero-means-equal-strings 100
  (prop/for-all [s1 (gen/not-empty gen/string)
                 s2 (gen/not-empty gen/string)]
    (if (= (str/distance s1 s2) 0)
      (= s1 s2)
      true)))

(defspec levenshtein-triangle 100
  (prop/for-all [s1 (gen/not-empty gen/string)
                 s2 (gen/not-empty gen/string)
                 s3 (gen/not-empty gen/string)]
    (<= (str/distance s1 s2)
        (+ (str/distance s1 s3)
           (str/distance s2 s3)))))

(defexamples distance-test
  (str/distance "foo" "foo") 0
  (str/distance "foo" "foo" :levenshtein) 0
  (str/distance "foo" "fo") 1
  (str/distance "karolin" "kathrin" :hamming) 3
  (str/distance "karolin" "kerstin" :hamming) 3
  (str/distance "1011101" "1001001" :hamming) 2
  (str/distance "2173896" "2233796" :hamming) 3
  (str/distance "foo" "foobar" :hamming) 3)

(deftest distance-throws-on-unknown
  (is (thrown? IllegalArgumentException (str/distance :unknown-algorithm))))

(defexamples longest-common-substring
  (str/longest-common-substring "fooquxbar" "foobar") #{"foo" "bar"}
  (str/longest-common-substring "FOOquxbar" "foobar") #{"bar"}
  (str/longest-common-substring "foo" "bar") #{})

(defspec finds-common-substring 100
  (prop/for-all [s1 (gen/such-that #(< (count %) 5) (gen/not-empty gen/string)
                                   1000)
                 s2 (gen/such-that #(< (count %) 5) (gen/not-empty gen/string)
                                   1000)
                 lcs (gen/such-that #(> (count %) 5) (gen/not-empty gen/string)
                                    1000)]
    (seq ((str/longest-common-substring (str s1 lcs) (str lcs s2)) lcs))))

(defspec length-test 100
  (prop/for-all [s gen/string]
    (= (.length s) (str/length s))))

(defexamples index-of-test
  (str/index-of "foo" "foo") 0
  (str/index-of "foo" "bar") nil)

(defexamples last-index-of-test
  (str/last-index-of "foo" "bar") nil
  (str/last-index-of "foofoo" "foo") 3)

(defspec char-at-acts-like-char-at
  (prop/for-all [[s i] (gen/bind (gen/not-empty gen/string)
                                 (fn [s]
                                   (gen/tuple
                                    (gen/return s)
                                    (gen/choose 0 (max 0 (dec (str/length s)))))))]
    (= (.charAt s i) (str/char-at s i))))

(defexamples re-quote
  (re-matches (re-pattern ".foo.") "afoob") "afoob"
  (re-matches (re-pattern (str/re-quote ".foo.")) "afoob") nil)

(deftest added-metadata-is-removed-from-aliased-vars
  (is (not (:added (meta #'str/trim)))))
