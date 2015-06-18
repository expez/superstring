(ns str.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check
             [clojure-test :refer [defspec]]
             [generators :as gen]
             [properties :as prop]]
            [str.core :as str]))

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

(deftest chomp
  (are [expected actual] (= expected actual)
    "" (str/chomp "" "")
    "" (str/chomp "foo" "foo")
    "foo"(str/chomp "foobar" "bar")
    "foo" (str/chomp "foo\n")
    "foo" (str/chomp "foo\r")
    "foo" (str/chomp "foo\r\n")
    "foo\n" (str/chomp "foo\n\r")))

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

(defspec ends-with?-can-ignore-case 100
  (prop/for-all [s gen/string
                 suffix gen/string]
    (str/ends-with? (str s suffix ) (str/swap-case suffix) :ignore-case)))

(defspec starts-with?-acts-like-startsWith 100
  (prop/for-all [s gen/string
                 prefix gen/string]
    (= (.startsWith (str prefix s) prefix)
       (if (str/starts-with? (str prefix s) prefix) true false))))

(defspec starts-with?-can-ignore-case 100
  (prop/for-all [s gen/string
                 prefix gen/string]
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

(defspec capitalized-strings-has-first-char-in-upper-case 100
  (prop/for-all [s gen/string
                 c gen/char-alpha]
    (Character/isUpperCase (first (str/capitalize (str c s))))))

(defn- lower-case-ascii-if-letter?
  ([] true)
  ([c] (if (Character/isLetter c) (Character/isLowerCase c) true))
  ([acc c] (and acc (lower-case-ascii-if-letter? c))))

(defspec capitalized-strings-has-rest-of-chars-in-lower-case 100
  (prop/for-all [s gen/string
                 c gen/char-alpha]
    (reduce lower-case-ascii-if-letter?
            (.substring (str/capitalize (str c s)) 1))))

(deftest capitalize
  (are [expected actual] (= expected actual)
    "Foo" (str/capitalize "foo" )
    "Foo" (str/capitalize "FOO")
    "Foo" (str/capitalize "FoO")
    "Foo" (str/capitalize "FoO")
    "Foo." (str/capitalize "FoO.")))

(defspec swap-case-does-not-change-length 100
  (prop/for-all [s gen/string]
    (= (.length (str/swap-case s)) (.length s))))

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
  (prop/for-all [s gen/string]
    (let [cases1 (map case-to-int s)
          cases2 (map case-to-int (str/swap-case s))]
      (apply = 0 (map + cases1 cases2)))))

(deftest swap-case
  (are [expected actual] (= expected actual)
    "Foo" (str/swap-case "fOO" )
    "foo" (str/swap-case "FOO")
    "åÅBERG" (str/swap-case "Ååberg")
    "æÆå." (str/swap-case "ÆæÅ.")))

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

(deftest slice
  (are [expected actual] (= expected actual)
    nil (str/slice "" 0)
    nil (str/slice "" 1)
    nil (str/slice "12" 1 -1)
    "1" (str/slice "1" 0)
    "2" (str/slice "12" 1)
    "12" (str/slice "12" 0 2)
    "3456" (str/slice "0123456" 3 100)
    "6" (str/slice "0123456" -1)
    "45" (str/slice "0123456" -3 2)
    "456" (str/slice "0123456" -3 100)))

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

(deftest right-pad
  (are [expected actual] (= expected actual)
    "" (str/pad-right "" 0)
    " " (str/pad-right "" 1)
    "foo "(str/pad-right "foo" 4)
    "foo  " (str/pad-right "foo" 5)
    "foo.!" (str/pad-right "foo" 5 ".!")
    "foo.!." (str/pad-right "foo" 6 ".!")))

(deftest left-pad
  (are [expected actual] (= expected actual)
    "" (str/pad-left "" 0)
    " " (str/pad-left "" 1)
    " foo"(str/pad-left "foo" 4)
    "  foo" (str/pad-left "foo" 5)
    ".!foo" (str/pad-left "foo" 5 ".!")
    ".!.foo" (str/pad-left "foo" 6 ".!")))

(defspec left-pad-results-in-strings-with-new-width 100
  (prop/for-all
      [[s width]
       (gen/bind gen/string
                 (fn [s]
                   (gen/tuple (gen/return s)
                              (gen/such-that #(> % (.length s)) gen/pos-int 100))))]
    (= (.length (str/pad-left s width)) width)))

(deftest center
  (are [expected actual] (= expected actual)
    "" (str/center "" 0)
    " " (str/center "" 1)
    "foo "(str/center "foo" 4)
    " foo " (str/center "foo" 5)
    "foo.!" (str/center "foo" 5 ".!")
    "foo." (str/center "foo" 4 ".!")
    ".!foo." (str/center "foo" 6 ".!")
    ".!foo.!." (str/center "foo" 8 ".!")))

(defspec center-results-in-strings-with-new-width 100
  (prop/for-all
      [[s width]
       (gen/bind gen/string
                 (fn [s]
                   (gen/tuple (gen/return s)
                              (gen/such-that #(> % (.length s)) gen/pos-int 100))))]
    (= (.length (str/center s width)) width)))

(deftest chop-suffix
  (are [expected actual] (= expected actual)
    "" (str/chop-suffix "" "foo")
    "" (str/chop-suffix "foo" "foo")
    "foo"(str/chop-suffix "foobar" "bar")
    "foo" (str/chop-suffix "foo " " ")
    "foo" (str/chop-suffix "foo" "bar")
    "foo" (str/chop-suffix "foo" "FOO")
    "foo" (str/chop-suffix "foo" "O")
    "fooÅ" (str/chop-suffix "fooÅ" "å")
    "" (str/chop-suffix "foo" "FOO" :ignore-case)
    "fo" (str/chop-suffix "foo" "O" :ignore-case)
    "foo" (str/chop-suffix "fooÅ" "å" :ignore-case)))

(deftest chop-prefix
  (are [expected actual] (= expected actual)
    "" (str/chop-prefix "" "foo")
    "" (str/chop-prefix "foo" "foo")
    "bar"(str/chop-prefix "foobar" "foo")
    "foo" (str/chop-prefix " foo" " ")
    "foo" (str/chop-prefix "foo" "bar")
    "foo" (str/chop-prefix "foo" "FOO")
    "foo" (str/chop-prefix "foo" "F")
    "Åfoo" (str/chop-prefix "Åfoo" "å")
    "" (str/chop-prefix "foo" "FOO" :ignore-case)
    "oo" (str/chop-prefix "foo" "F" :ignore-case)
    "foo" (str/chop-prefix "Åfoo" "Å" :ignore-case)))

(deftest contains-test?
  (are [expected actual] (= expected actual)
    "" (str/contains? "" "")
    "1" (str/contains? "1" "1")
    "foo" (str/contains? "foo" "fo")
    nil (str/contains? "foobar" "qux")
    nil (str/contains? "foobar" "BAR")
    "foobar" (str/contains? "foobar" "BAR" :ignore-case)
    "Albert Åberg"(str/contains? "Albert Åberg" "åberg" :ignore-case)))

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

(deftest contains-all?
  (are [expected actual] (= expected actual)
    "" (str/contains-all? "" [])
    "" (str/contains-all? "" [""])
    nil (str/contains-all? "" [nil])
    nil (str/contains-all? nil [""])
    "12" (str/contains-all? "12" ["1" "2"])
    "foo" (str/contains-all? "foo" ["fo" "o"])
    nil (str/contains-all? "foobar" ["qux"])
    nil (str/contains-all? "foobar" ["foo" "qux"])
    nil (str/contains-all? "foobar" ["BAR"])
    "foobar" (str/contains-all? "foobar" ["BAR" "Foo"] :ignore-case)
    "Albert Åberg"(str/contains-all? "Albert Åberg" ["åberg" "al"] :ignore-case)))

(defspec contains-all? 100
  (prop/for-all [s1 gen/string
                 s2 gen/string
                 s3 gen/string]
    (str/contains-all? (str s1 s2 s3) [s1 s2 s3])))

(defspec contains-any-finds-a-needle 100
  (prop/for-all [before (gen/not-empty gen/string)
                 needle (gen/not-empty gen/string)
                 after (gen/not-empty gen/string)]
    (is (str/contains-any? (str before needle after) [needle]))))

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

(deftest truncate
  (are [expected actual] (= expected actual)
    "" (str/truncate "" 3)
    "..." (str/truncate "123456" 3)
    "123..." (str/truncate "123456" 6)
    "1" (str/truncate "1" 3)
    "12" (str/truncate "12" 3)
    "123" (str/truncate "123" 3)))

(deftest common-prefix
  (are [expected actual] (= expected actual)
    "" (str/common-prefix "321" "123")
    "Åffø" (str/common-prefix "Åffø123456" "Åfføo8yuidfg")
    "123" (str/common-prefix "123456" "123o8yuidfg")
    "" (str/common-prefix "Åberg" "åberg")
    "Åberg" (str/common-prefix "åberg" "Åberg" :ignore-case)
    "åberg" (str/common-prefix "Åberg" "åberg" :ignore-case)))

(defspec common-prefix-finds-common-prefixes 100
  (prop/for-all [prefix (gen/not-empty gen/string)
                 s1 gen/string
                 s2 gen/string]
    (.startsWith (str/common-prefix (str prefix s1) (str prefix s2)) prefix)))


(deftest common-suffix
  (are [expected actual] (= expected actual)
    "" (str/common-suffix "321" "123")
    "Åffø" (str/common-suffix "123456Åffø" "o8yuidfgÅffø")
    "123" (str/common-suffix "456123" "o8yuidfg123")
    "" (str/common-suffix "åberG" "åberg")
    "åberG" (str/common-suffix "åberg" "åberG" :ignore-case)
    "åberg" (str/common-suffix "Åberg" "åberg" :ignore-case)))

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
    (is (upper-if-upper-exists? (first (str/title-case s))))))

(defspec title-case-rest-is-all-lower-case 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (is (reduce (fn [acc c] (and acc (lower-if-lower-exists? c))) true
                (rest (str/title-case s))))))

(defspec upper-case?-returns-true-on-all-upper 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (is (str/upper-case? (str/upper-case s)))))

(defspec upper-case?-returns-nil-on-all-lower 100
  (prop/for-all [s (gen/such-that not-empty
                                  (gen/fmap str/join (gen/vector gen/char-alpha)))]
    (is (not (str/upper-case? (str/lower-case s))))))

(deftest upper-case-test
  (are [expected actual] (= expected actual)
    "UPPER" (str/upper-case? "UPPER")
    "123UPPER!" (str/upper-case? "123UPPER!")))

(defspec lower-case?-returns-true-on-all-lower 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (is (str/lower-case? (str/lower-case s)))))

(defspec lower-case?-returns-nil-on-all-upper 100
  (prop/for-all [s (gen/such-that not-empty
                                  (gen/fmap str/join (gen/vector gen/char-alpha)))]
    (is (not (str/lower-case? (str/upper-case s))))))

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
    (is (reduce (fn [acc line]
                  (and acc (less-than-width-or-unbreakable line width)))
                true
                (str/split-lines (str/wrap-words (str/join " " words) width))))))

(deftest lower-case-test
  (are [expected actual] (= expected actual)
    "upper" (str/lower-case? "upper")
    "123upper!" (str/lower-case? "123upper!")))

(defspec lisp-case-is-all-lower 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (is (reduce (fn [acc c] (and acc (lower-if-lower-exists? c))) true
                (str/lisp-case s)))))

(deftest lisp-case-test
  (are [expected actual] (= expected actual)
    "pascal-case" (str/lisp-case "PascalCase")
    "set-id" (str/lisp-case "setID")
    "http-request" (str/lisp-case "HTTPRequest")
    "snake-case" (str/lisp-case "snake_case")
    "screaming-snake-case" (str/lisp-case "SCREAMING_SNAKE_CASE")))

(deftest camel-case-test
  (are [expected actual] (= expected actual)
    "pascalCase" (str/camel-case "PascalCase")
    "setId" (str/camel-case "setID")
    "httpRequest" (str/camel-case "HTTPRequest")
    "snakeCase" (str/camel-case "snake_case")
    "screamingSnakeCase" (str/camel-case "SCREAMING_SNAKE_CASE")))

(deftest pascal-case-test
  (are [expected actual] (= expected actual)
    "PascalCase" (str/pascal-case "PascalCase")
    "SetId" (str/pascal-case "setID")
    "HttpRequest" (str/pascal-case "HTTPRequest")
    "SnakeCase" (str/pascal-case "snake_case")
    "ScreamingSnakeCase" (str/pascal-case "SCREAMING_SNAKE_CASE")))

(deftest snake-case-test
  (are [expected actual] (= expected actual)
    "pascal_case" (str/snake-case "PascalCase")
    "set_id" (str/snake-case "setID")
    "http_request" (str/snake-case "HTTPRequest")
    "snake_case" (str/snake-case "snake_case")
    "screaming_snake_case" (str/snake-case "SCREAMING_SNAKE_CASE")))

(deftest screaming-snake-case-test
  (are [expected actual] (= expected actual)
    "PASCAL_CASE" (str/screaming-snake-case "PascalCase")
    "SET_ID" (str/screaming-snake-case "setID")
    "HTTP_REQUEST" (str/screaming-snake-case "HTTPRequest")
    "SNAKE_CASE" (str/screaming-snake-case "snake_case")
    "SCREAMING_SNAKE_CASE" (str/screaming-snake-case "SCREAMING_SNAKE_CASE")))

(deftest strip-accents-test
  (are [expected actual] (= expected actual)
    "Et ca sera sa moitie" (str/strip-accents "Et ça sera sa moitié")
    "aaaaaaaæaccceeeeeghiiiijlnnooooooøssssttuuuuuunyyczzz"
    (str/strip-accents "ąàáäâãåæăćčĉęèéëêĝĥìíïîĵľńňòóöőôõøśșšŝťțŭùúüűûñÿýçżźž")))

(def string-ascii
  (gen/fmap #(apply str %) (gen/vector gen/char-alpha)))

(defspec ascii?-returns-s-on-ascii-strings 100
  (prop/for-all [s (gen/not-empty gen/string-ascii)]
    (is (= s (str/ascii? s)))))

(deftest ascii?-test
  (are [expected actual] (= expected actual)
    "ascii" (str/ascii? "ascii")
    nil (str/ascii? "Et ça sera sa moitié")))

(defspec slug-contains-only-ascii-chars 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (is (str/ascii? (str/slug s)))))

(defspec slug-contains-no-whitespace 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (is (not (re-find #"\s+" (str/slug s))))))

(defspec slug-is-all-lower 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (is (str/lower-case? (str/slug s)))))

(defspec slug-only-contains-unreserved-characters 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (is (not (re-matches #"[^A-Za-z0-9_.~-]+" (str/slug s))))))

(deftest slug-test
  (are [expected actual] (= expected actual)
    "this-that-the-other-various-outre-considerations"
    (str/slug "This, That & the Other! Various Outré   Considerations")))

(defspec mixed-case-is-true-for-mixed-case-strings 100
  (prop/for-all [s1 (gen/not-empty gen/string)
                 s2 (gen/not-empty gen/string)]
    (is (str/mixed-case? (str (str/upper-case s1) (str/lower-case s2))))))

(defspec mixed-case-is-false-for-upper-case-strings 100
  (prop/for-all [s string-ascii]
    (is (not (str/mixed-case? (str/upper-case s))))))

(deftest mixed-case?-test
  (are [expected actual] (= expected actual)
    "FooBar" (str/mixed-case? "FooBar")
    nil (str/mixed-case? "foo")))

(def whitespace
  (gen/fmap #(apply str %)
            (gen/not-empty (gen/vector
                            (gen/elements [\tab \newline \return \space])))))

(defspec collapse-whitespace-collapses-whitespace 100
  (prop/for-all [s (gen/not-empty gen/string-alphanumeric)
                 ws1 (gen/not-empty whitespace)
                 ws2 (gen/not-empty whitespace)]
    (is (= (+ (.length s) 2)
           (.length (str/collapse-whitespace (str ws1 s ws2)))))))

(deftest collapse-whitespace-test
  (are [expected actual] (= expected actual)
    "foo bar baz" (str/collapse-whitespace "foo

bar    	baz")
    " foo bar baz " (str/collapse-whitespace " foo bar baz ")))

(defspec lehvenstein-distance-is-at-least-difference-between-string-lenghts 100
  (prop/for-all [s1 (gen/not-empty gen/string)
                 s2 (gen/not-empty gen/string)]
    (is (>= (str/distance s1 s2)
            (- (max (.length s1) (.length s2))
               (min (.length s1) (.length s2)))))))

(defspec lehvenstein-distance-is-at-most-length-of-longest-string 100
  (prop/for-all [s1 (gen/not-empty gen/string)
                 s2 (gen/not-empty gen/string)]
    (is (<= (str/distance s1 s2)
            (max (.length s1) (.length s2))))))

(defspec lehvenstein-distance-is-zero-for-equal-strings 100
  (prop/for-all [s (gen/not-empty gen/string)]
    (is (= 0 (str/distance s s)))))

(defspec lehvenstein-distance-is-zero-means-equal-strings 100
  (prop/for-all [s1 (gen/not-empty gen/string)
                 s2 (gen/not-empty gen/string)]
    (if (= (str/distance s1 s2) 0)
      (is (= s1 s2))
      true)))

(defspec lehvenstein-triangle 100
  (prop/for-all [s1 (gen/not-empty gen/string)
                 s2 (gen/not-empty gen/string)
                 s3 (gen/not-empty gen/string)]
    (<= (str/distance s1 s2)
        (+ (str/distance s1 s3)
           (str/distance s2 s3)))))

(deftest distance-test
  (are [expected actual] (= expected actual)
    0 (str/distance "foo" "foo")
    0 (str/distance "foo" "foo" :levenshtein)
    1 (str/distance "foo" "fo")
    3 (str/distance "karolin" "kathrin" :hamming)
    3 (str/distance "karolin" "kerstin" :hamming)
    2 (str/distance "1011101" "1001001" :hamming)
    3 (str/distance "2173896" "2233796" :hamming)
    3 (str/distance "foo" "foobar" :hamming)))

(deftest distance-throws-on-unknown
  (is (thrown? IllegalArgumentException (str/distance :unknown-algorithm))))

(deftest longest-common-substrings
  (are [expected actual] (= expected actual)
    #{"foo" "bar"} (str/longest-common-substrings "fooquxbar" "foobar")
    #{"bar"} (str/longest-common-substrings "FOOquxbar" "foobar")
    #{} (str/longest-common-substrings "foo" "bar")))

(defspec finds-common-substring 100
  (prop/for-all [s1 (gen/such-that #(< (count %) 5) (gen/not-empty gen/string)
                                   500)
                 s2 (gen/such-that #(< (count %) 5) (gen/not-empty gen/string)
                                   500)
                 lcs (gen/such-that #(> (count %) 5) (gen/not-empty gen/string)
                                    500)]
    (is ((str/longest-common-substrings (str s1 lcs) (str lcs s2)) lcs))))
