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
  (prop/for-all [vals (gen/bind (gen/not-empty gen/string)
                                (fn [sep]
                                  (gen/tuple (gen/return sep)
                                             (gen/such-that #(not (.endsWith % sep))
                                                            gen/string))))]
    (let [sep (first vals)
          s (second vals)]
      (= (count (str/chomp s sep)) (count s)))))

(defspec chomp-removes-newline 100
  (prop/for-all [s gen/string]
    (= (.length (str/chomp (str s "\n")))) (.length s)))

(defspec chomp-removes-carriage-return 100
  (prop/for-all [s gen/string]
    (= (.length (str/chomp (str s "\r"))) (.length s))))

(defspec chomp-removes-carriage-return-line-feed 100
  (prop/for-all [s gen/string]
    (= (.length (str/chomp (str s "\r\n"))) (.length s))))

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
    (str/ends-with? (str s suffix ) (str/invert-case suffix) :ignore-case)))

(defspec starts-with?-acts-like-startsWith 100
  (prop/for-all [s gen/string
                 prefix gen/string]
    (= (.startsWith (str prefix s) prefix)
       (if (str/starts-with? (str prefix s) prefix) true false))))

(defspec starts-with?-can-ignore-case 100
  (prop/for-all [s gen/string
                 prefix gen/string]
    (str/starts-with? (str prefix s) (str/invert-case prefix) :ignore-case)))

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

(defspec invert-case-does-not-change-length 100
  (prop/for-all [s gen/string]
    (= (.length (str/invert-case s)) (.length s))))

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

(defspec invert-case-changes-case 100
  (prop/for-all [s gen/string]
    (let [cases1 (map case-to-int s)
          cases2 (map case-to-int (str/invert-case s))]
      (apply = 0 (map + cases1 cases2)))))

(defspec slice-without-end-has-length-1 100
  (prop/for-all [vals (gen/bind (gen/not-empty gen/string)
                                (fn [s]
                                  (gen/tuple (gen/return s)
                                             (gen/such-that #(< % (.length s))
                                                            gen/int 100))))]
    (let [s (first vals)
          i (second vals)]
      (= (.length (str/slice s 0)) 1))))

(defspec slice-with-length-outside-string 100
  ;; When beg + end falls outside the string we return the rest of the
  ;; string starting from beg
  (prop/for-all [s (gen/not-empty gen/string)]
    (let [beg (rand-int (.length s))]
      (= (str/slice s beg (+ (.length s) beg)) (.substring s beg)))))

(defspec slices-with-index-outside-str-is-nil 100
  (prop/for-all [vals (gen/bind (gen/not-empty gen/string)
                                (fn [s]
                                  (gen/tuple
                                   (gen/return s)
                                   (gen/such-that #(> (Math/abs %) (.length s))
                                                  gen/int 100))))]
    (let [s (first vals)
          index (second vals)
          len (inc (rand-int (dec (.length s))))]
      (nil? (str/slice s index len)))))

(defspec slices-with-negative-lengths-are-nil 100
  (prop/for-all [len (gen/such-that #(not (zero? %)) gen/neg-int)
                 s gen/string
                 index gen/int]
    (nil? (str/slice s index len))))

(deftest slice-some-strings
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

(deftest right-pad-some-strings
  (are [expected actual] (= expected actual)
    "" (str/pad-right "" 0)
    " " (str/pad-right "" 1)
    "foo "(str/pad-right "foo" 4)
    "foo  " (str/pad-right "foo" 5)
    "foo.!" (str/pad-right "foo" 5 ".!")
    "foo.!." (str/pad-right "foo" 6 ".!")))

(deftest left-pad-some-strings
  (are [expected actual] (= expected actual)
    "" (str/pad-left "" 0)
    " " (str/pad-left "" 1)
    " foo"(str/pad-left "foo" 4)
    "  foo" (str/pad-left "foo" 5)
    ".!foo" (str/pad-left "foo" 5 ".!")
    ".!.foo" (str/pad-left "foo" 6 ".!")))

(defspec left-pad-results-in-strings-with-new-width 100
  (prop/for-all
      [vals
       (gen/bind gen/string
                 (fn [s]
                   (gen/tuple (gen/return s)
                              (gen/such-that #(> % (.length s)) gen/pos-int 100))))]
    (let [s (first vals)
          width (second vals)]
      (= (.length (str/pad-left s width)) width))))
