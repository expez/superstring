(ns str.core-test
  (:require [clojure.string :as str]
            [clojure.test.check
             [clojure-test :refer [defspec]]
             [generators :as gen]
             [properties :as prop]]
            [clojure.test :refer :all]
            [str.core :refer :all]))

(defspec appending-separator-and-chomping-does-not-alter-length 100
  (prop/for-all [s gen/string
                 sep gen/string]
    (let [res (chomp s)]
      (= (count (chomp (str s sep) sep))
         (count s)))))

(defspec chomping-string-not-ending-in-seperator-does-not-alter-length 100
  (prop/for-all [sep gen/string]
    (prop/for-all [s (gen/such-that #(not (.endsWith % sep)) gen/string)]
      (= (count (chomp s sep)) (count s)))))

(defspec chomp-removes-newline 100
  (prop/for-all [s gen/string]
    (= (.length (chomp (str s "\n")))) (.length s)))

(defspec chomp-removes-carriage-return 100
  (prop/for-all [s gen/string]
    (= (.length (chomp (str s "\r"))) (.length s))))

(defspec chomp-removes-carriage-return-line-feed 100
  (prop/for-all [s gen/string]
    (= (.length (chomp (str s "\r\n"))) (.length s))))

(defspec ends-with?-acts-like-endsWith 100
  (prop/for-all [s gen/string
                 suffix gen/string]
    (= (.endsWith (str s suffix) suffix)
       (if (ends-with? (str s suffix) suffix) true false))))

(defspec starts-with?-acts-like-startsWith 100
  (prop/for-all [s gen/string
                 prefix gen/string]
    (= (.startsWith (str prefix s) prefix)
       (if (starts-with? (str prefix s) prefix) true false))))

(defspec chop-reduces-length-by-1-without-cr 100
  (prop/for-all [s (gen/such-that #(not (ends-with? % "\r\n")) gen/string)]
    (= (.length (chop s)) (max 0 (dec (.length s))))))

(defspec chop-gets-rid-of-both-chars-in-crlf 100
  (prop/for-all [s gen/string]
    (let [s (str s "\r\n")]
      (= (.length (chop s)) (max 0 (- (.length s) 2))))))

(deftest chopping-the-empty-string-is-a-no-op []
  (is (= (chop "") "")))

(defspec capitalized-strings-has-first-char-in-upper-case 100
  (prop/for-all [s gen/string
                 c gen/char-alpha]
    (Character/isUpperCase (first (capitalize (str c s))))))

(defn- lower-case-ascii-if-letter?
  ([] true)
  ([c] (if (Character/isLetter c) (Character/isLowerCase c) true))
  ([acc c] (and acc (lower-case-ascii-if-letter? c))))

(defspec capitalized-strings-has-rest-of-chars-in-lower-case 100
  (prop/for-all [s gen/string
                 c gen/char-alpha]
    (reduce lower-case-ascii-if-letter?
            (.substring (capitalize (str c s)) 1))))
