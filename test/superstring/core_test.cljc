(ns superstring.core-test
  #?@(:cljs [(:require
              [clojure.test :as t :include-macros true]
              [clojure.test.check.clojure-test :refer-macros [defspec]]
              [clojure.test.check.generators :as gen]
              [clojure.test.check.properties :as prop :include-macros true]
              [superstring.core :as str])
             (:require-macros [superstring.test-helpers :refer [defexamples]])]
      :clj [(:require [clojure.test :as t]
                      [clojure.test.check.clojure-test :refer [defspec]]
                      [clojure.test.check.generators :as gen]
                      [clojure.test.check.properties :as prop]
                      [superstring.test-helpers :refer [defexamples]]
                      [superstring.core :as str])]))

(defspec length-test 100
  (prop/for-all [s gen/string]
    (= #?(:cljs (.-length s) :clj (.length s)) (str/length s))))

(defexamples index-of-test
  (str/index-of "foo" "foo") 0
  (str/index-of "foo" "bar") nil)

(defexamples last-index-of-test
  (str/last-index-of "foo" "bar") nil
  (str/last-index-of "foofoo" "foo") 3)

(defspec ends-with?-finds-endings 100
  (prop/for-all [s gen/string
                 suffix gen/string]
    (str/ends-with? (str s suffix) suffix)))

(def string-without-german-b (gen/such-that (fn [s] (not-any? #(= % \ß) s))
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

(defspec starts-with?-can-ignore-case 100
  (prop/for-all [s gen/string
                 prefix string-without-german-b]
    (str/starts-with? (str prefix s) (str/swap-case prefix) :ignore-case)))

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
                                      (gen/such-that (fn [s]
                                                       (not (str/ends-with? s sep)))
                                                     gen/string))))]
    (= (count (str/chomp s sep)) (count s))))

(defspec chomp-removes-newline 100
  (prop/for-all [s gen/string]
    (= (str/length (str/chomp (str s "\n")))) (str/length s)))

(defspec chomp-removes-carriage-return 100
  (prop/for-all [s gen/string]
    (= (str/length (str/chomp (str s "\r"))) (str/length s))))

(defspec chomp-removes-carriage-return-line-feed 100
  (prop/for-all [s gen/string]
    (= (str/length (str/chomp (str s "\r\n"))) (str/length s))))

(defexamples chomp-examples
  (str/chomp "" "") ""
  (str/chomp "foo" "foo") ""
  (str/chomp "foobar" "bar") "foo"
  (str/chomp "foo\n") "foo"
  (str/chomp "foo\r") "foo"
  (str/chomp "foo\r\n") "foo"
  (str/chomp "foo\n\r") "foo\n")

(defspec chop-reduces-length-by-1-without-cr 100
  (prop/for-all [s (gen/such-that #(not (str/ends-with? % "\r\n")) gen/string)]
    (= (str/length (str/chop s)) (max 0 (dec (str/length s))))))

(defspec chop-gets-rid-of-both-chars-in-crlf 100
  (prop/for-all [s gen/string]
    (let [s (str s "\r\n")]
      (= (str/length (str/chop s)) (max 0 (- (str/length s) 2))))))

(t/deftest chopping-the-empty-string-is-a-no-op []
  (t/is (= (str/chop "") "")))

(defspec swap-case-does-not-change-length 100
  (prop/for-all [s string-without-german-b]
    (= (str/length (str/swap-case s)) (str/length s))))

(defspec swap-case-changes-case 100
  (prop/for-all [s string-without-german-b]
    (let [count-uppers (partial reduce (fn [acc c] (if (str/upper-case? (str c))
                                                     (inc acc) 0)) 0)
          count-lowers (partial reduce (fn [acc c] (if (str/lower-case? (str c))
                                                     (inc acc) 0)) 0)]
      (and (= (count-uppers s) (count-lowers (str/swap-case s)))
           (= (count-lowers s) (count-uppers (str/swap-case s)))))))

(defexamples swap-case
  (str/swap-case "fOO") "Foo"
  (str/swap-case "FOO") "foo"
  (str/swap-case "Ååberg") "åÅBERG"
  (str/swap-case "ÆæÅ.") "æÆå."
  (str/swap-case "ß") "SS")

(defspec slice-without-end-has-length-1 100
  (prop/for-all [[s i] (gen/bind (gen/not-empty gen/string)
                                 (fn [s]
                                   (gen/tuple (gen/return s)
                                              (gen/such-that #(< % (str/length s))
                                                             gen/int 100))))]
    (= (str/length (str/slice s 0)) 1)))

(defspec slice-with-length-outside-string 100
  ;; When beg + end falls outside the string we return the rest of the
  ;; string starting from beg
  (prop/for-all [s (gen/not-empty gen/string)]
    (let [beg (rand-int (str/length s))]
      (= (str/slice s beg (+ (str/length s) beg))
         (str/substring s beg)))))

(defspec slices-with-index-outside-str-is-nil 100
  (prop/for-all [[s index] (gen/bind (gen/not-empty gen/string)
                                     (fn [s]
                                       (gen/tuple
                                        (gen/return s)
                                        (gen/such-that #(> (Math/abs %) (str/length s))
                                                       gen/int 100))))]
    (let [len (inc (rand-int (dec (str/length s))))]
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
  (str/slice "0123456" -3 100) "456"
  (superstring.core/slice "012345678901234567890123456789"  20 11) "0123456789")

(defspec right-pad-results-in-strings-with-new-width 100
  (prop/for-all
      [vals
       (gen/bind gen/string
                 (fn [s]
                   (gen/tuple (gen/return s)
                              (gen/such-that #(> % (str/length s)) gen/pos-int 100))))]
    (let [s (first vals)
          width (second vals)]
      (= (str/length (str/pad-right s width)) width))))

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
                              (gen/such-that #(> % (str/length s)) gen/pos-int 100))))]
    (= (str/length (str/pad-left s width)) width)))

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
                              (gen/such-that #(> % (str/length s)) gen/pos-int 100))))]
    (= (str/length (str/center s width)) width)))

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
  (str/includes? "" "") ""
  (str/includes? "1" "1") "1"
  (str/includes? "foo" "fo") "foo"
  (str/includes? "foobar" "qux") nil
  (str/includes? "foobar" "BAR") nil
  (str/includes? "foobar" "BAR" :ignore-case) "foobar"
  (str/includes? "fooß" "ss" :ignore-case) nil
  (str/includes? "fooß" "SS" :ignore-case) nil
  (str/includes? "ß" "SS" :ignore-case) nil
  (str/includes? "Albert Åberg" "åberg" :ignore-case) "Albert Åberg")

(defspec includes?-finds-generated-strings 100
  (prop/for-all [before gen/string
                 needle (gen/not-empty gen/string)
                 after gen/string]
    (str/includes? (str before needle after) needle)))

(defn- randomly-swapcase
  [s]
  (let [swapcase (fn [c]
                   #?(:cljs (if (str/upper-case? c)
                              (str/lower-case c)
                              (if (str/lower-case? c)
                                (str/upper-case c)
                                c))
                      :clj (if (Character/isUpperCase c)
                             (Character/toLowerCase c)
                             (if (Character/isLowerCase c)
                               (Character/toUpperCase c)
                               c))))]
    (apply str (map #(if (> (rand-int 3) 1) (swapcase %) %) s))))


(defspec includes?-can-ignore-case 100
  (prop/for-all [before gen/string
                 needle (gen/fmap randomly-swapcase (gen/not-empty gen/string))
                 after gen/string]
    (str/includes? (str before needle after) needle :ignore-case)))

(defexamples includes-all?
  (str/includes-all? "" []) ""
  (str/includes-all? "" [""]) ""
  (str/includes-all? "12" ["1" "2"]) "12"
  (str/includes-all? "foo" ["fo" "o"]) "foo"
  (str/includes-all? "foobar" ["qux"]) nil
  (str/includes-all? "foobar" ["foo" "qux"]) nil
  (str/includes-all? "foobar" ["BAR"]) nil
  (str/includes-all? "foobar" ["BAR" "Foo"] :ignore-case) "foobar"
  (str/includes-all? "Albert Åberg" ["åberg" "al"] :ignore-case) "Albert Åberg")

(defspec includes-any?-finds-a-needle 100
  (prop/for-all [before (gen/not-empty gen/string)
                 needle (gen/not-empty gen/string)
                 after (gen/not-empty gen/string)]
    (str/includes-any? (str before needle after) [needle])))

(defexamples includes-any?
  (str/includes-any? "foobar" ["foo"]) "foobar"
  (str/includes-any? "foobar" ["qux"]) nil
  (str/includes-any? "foobar" ["qux" "bar"]) "foobar"
  (str/includes-any? "ß" ["ss" "SS"] :ignore-case) nil
  (str/includes-any? "foobar" ["BAR"] :ignore-case) "foobar")

(defspec contains-any-can-ignore-case 100
  (prop/for-all [before (gen/not-empty gen/string)
                 needle (gen/not-empty string-without-german-b)
                 after (gen/not-empty gen/string)]
    (-> before
        (str (randomly-swapcase needle) after)
        (str/includes-any? [needle] :ignore-case))))

(defspec truncated-strings-have-right-length 100
  (prop/for-all [[s len] (gen/bind (gen/such-that #(> (str/length %) 3) gen/string 100)
                                   (fn [s]
                                     (gen/tuple
                                      (gen/return s)
                                      (gen/choose 3 (max 4 (- (str/length s) 3)) ))))]
    (= (str/length (str/truncate s len)) (min (str/length s) len))))

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
    (str/starts-with? (str/common-prefix (str prefix s1) (str prefix s2)) prefix)))

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
    (str/ends-with? (str/common-suffix (str s1 suffix) (str s2 suffix)) suffix)))

(defspec upper-case?-returns-true-on-all-upper 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (str/upper-case? (str/upper-case s))))

(defspec upper-case?-returns-nil-on-all-lower 100
  (prop/for-all [s (gen/such-that not-empty
                                  (gen/fmap str/join (gen/vector gen/char-alpha)))]
    (not (str/upper-case? (str/lower-case s)))))

(t/deftest upper-case-test
  (str/upper-case? "UPPER") "UPPER"
  (str/upper-case? "123UPPER!") "123UPPER!")

(defexamples lower-case-test
  (str/lower-case? "upper") "upper"
  (str/lower-case? "123upper!") "123upper!")

(defspec lower-case?-returns-true-on-all-lower 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (str/lower-case? (str/lower-case s))))

(defspec lower-case?-returns-nil-on-all-upper 100
  (prop/for-all [s (gen/such-that not-empty
                                  (gen/fmap str/join (gen/vector gen/char-alpha)))]
    (not (str/lower-case? (str/upper-case s)))))

(defn- word []
  (gen/fmap #(if (< (str/length %) 10)
               % (str/substring % 0 (rand-int (str/length %))))
            (gen/not-empty gen/string-alphanumeric)))

(defn- less-than-width-or-unbreakable [line width]
  (or (<= (str/length line) width)
      (not (re-find #" " line))))

(defspec wrap-words-have-lines-no-longer-than-max-width 25
  (prop/for-all [words (gen/bind (gen/choose 50 500)
                                 #(gen/return (gen/sample (word) %)))
                 width (gen/choose 8 80)]
    (reduce (fn [acc line]
              (and acc (less-than-width-or-unbreakable line width)))
            true
            (str/split-lines (str/wrap-words (str/join " " words) width)))))

(defspec lisp-case-is-all-lower 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (str/lower-case? (str/lisp-case s))))

(defexamples lisp-case-test
  (str/lisp-case "abcAbc?abc") "abc-abc?abc"
  (str/lisp-case "abcAbc!") "abc-abc!"
  (str/lisp-case "abcAbc*abc") "abc-abc*abc"
  (str/lisp-case "123Abc") "123-abc"
  (str/lisp-case "123aBc") "123a-bc"
  (str/lisp-case "PascalCase") "pascal-case"
  (str/lisp-case "setID") "set-id"
  (str/lisp-case "HTTPRequest") "http-request"
  (str/lisp-case "snake_case") "snake-case"
  (str/lisp-case "message(SMS)") "message(sms)"
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
  "this-that-the-other-various-outre-considerations"
  (str/slug "ąćęółńśźż") "aceolnszz"
  (str/slug "FÖÖBAR")  "foobar")

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
    (= (+ (str/length s) 2)
       (str/length (str/collapse-whitespace (str ws1 s ws2))))))

(defexamples collapse-whitespace-test
  (str/collapse-whitespace "foo

bar          baz") "foo bar baz"
  (str/collapse-whitespace " foo bar baz ") " foo bar baz ")

(defspec levenshtein-distance-is-at-least-difference-between-string-lenghts 100
  (prop/for-all [s1 (gen/not-empty gen/string)
                 s2 (gen/not-empty gen/string)]
    (>= (str/distance s1 s2)
        (- (max (str/length s1) (str/length s2))
           (min (str/length s1) (str/length s2))))))

(defspec levenshtein-distance-is-at-most-length-of-longest-string 100
  (prop/for-all [s1 (gen/not-empty gen/string)
                 s2 (gen/not-empty gen/string)]
    (<= (str/distance s1 s2)
        (max (str/length s1) (str/length s2)))))

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

(t/deftest distance-throws-on-unknown
  (t/is #?(:cljs (thrown? js/Error
                          (str/distance "s1" "s2" :unknown-algorithm))
           :clj (thrown? IllegalArgumentException (str/distance :unknown-algorithm)))))

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

#?(:clj
   (t/deftest added-metadata-is-removed-from-aliased-vars
     (t/is (not (:added (meta #'str/trim))))))

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

(defexamples translate-test
  (str/translate "abba" {\a \b}) "bbbb"
  (str/translate "abba" {\a \b \b \a}) "baab"
  (str/translate "foo" {\a \b }) "foo"
  (str/translate "gabba" {\a \b} #{\b}) "gbb"
  (str/translate "gabba" {\a nil} #{\b}) "g")

(defspec translate-deletes-all-unwanted-chars
  (prop/for-all [s (gen/not-empty gen/string)
                 ds (gen/vector gen/char 100)]
    (let [ds (distinct ds)
          tmap (apply hash-map (if (even? (count ds)) ds (drop 1 ds)))]
      (t/is (not (str/includes-any? s {} (map str (keys tmap))))))))

(defspec translate-translates-all-chars-in-tmap
  (prop/for-all [s (gen/not-empty gen/string)
                 ds (gen/vector gen/char 10)]
    (let [ds (distinct ds)
          ds (if (even? (count ds)) ds (drop 1 ds))
          tmap (apply hash-map ds)]
      (t/is (not (str/includes-any? (str/translate s tmap)
                                    (map str (keys tmap))))))))

(t/deftest added-metadata-is-removed-from-aliased-vars
  (t/is (not (:added (meta #'str/trim)))))

#?(:clj
   (do
     (t/deftest starts-with?-handles-CharSequence
       (t/is (str/starts-with? (StringBuffer. "foobar") "foo"))
       (t/is (string? (str/starts-with? (StringBuffer. "foobar") "foo"))))
     (t/deftest ends-with?-handles-CharSequence
       (t/is (str/ends-with? (StringBuffer. "foobar") "bar"))
       (t/is (string? (str/ends-with? (StringBuffer. "foobar") "bar"))))
     (t/deftest includes?-handles-CharSequence
       (t/is (str/includes? (StringBuffer. "foobar") "bar"))
       (t/is (string? (str/includes? (StringBuffer. "foobar") "bar"))))))

(defexamples some?-test
  (str/some? "foo") "foo"
  (str/some? "") nil
  (str/some? nil) nil)

(defexamples replace-last-test
  (str/replace-last "foobarbar" "bar" "baz") "foobarbaz"
  (str/replace-last "foobarbar" #"bar" "baz") "foobarbaz"
  (str/replace-last "foobar" #"quz" "baz") "foobar"
  (str/replace-last "foobar" "quz" "baz") "foobar"
  ;; This example is slightly degenerate but I just want it to be the
  ;; same degenerate thing replace-first returns.
  (str/replace-last "foobar" "" "baz") "foobarbaz"
  (str/replace-last "foobarbar" #"bar" (constantly "baz")) "foobarbaz")

(defn- remove-regex-chars [s]
  (str/replace s "$" "a"))

(defspec replace-last-always-replaces-last 100
  (prop/for-all [suffix gen/string
                 match (gen/not-empty gen/string)
                 prefix (gen/not-empty gen/string)
                 replacement (gen/fmap remove-regex-chars gen/string)]
    (let [s (str prefix match suffix)
          correct-result (str/reverse (str/replace-first (str/reverse s)
                                                         (str/reverse match)
                                                         (str/reverse replacement)))]
      (t/is (= (str/replace-last s match replacement) correct-result)))))

#?(:clj
   (defexamples replace-last-clj-test
     (str/replace-last "foooar" \o \b) "foobar"))

#?(:cljs
   (do
     (enable-console-print!)
     (set! *main-cli-fn* #(t/run-tests))))
