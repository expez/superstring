(ns superstring.core
  (:require [clojure.string :as str]
            [clojure.set :as set])
  (:refer-clojure :exclude [reverse replace contains?]))

(declare slice)

(def blank? str/blank?)
(def capitalize str/capitalize)
(def join str/join)
(def lower-case str/lower-case)
(def escape str/escape)
(def replace str/replace)
(def reverse str/reverse)
(def split str/split)
(def split-lines str/split-lines)
(def trim str/trim)
(def trim-newline str/trim-newline)
(def triml str/triml)
(def trimr str/trimr)
(def upper-case str/upper-case)

(def substring clojure.core/subs)

(defn length
  "Return the length of s."
  [^String s]
  {:pre [(string? s)]
   :post [(integer? %)]}
  (.-length s))

(defn index-of
  "Return the starting position of the first occurrence of needle in s or nil.

  If start is provided, start the search at that position in s."
  ([^String s needle]
   {:pre [(string? s) (string? needle)]
    :post [(or (integer? %) (nil? %))]}
   (index-of s needle 0))
  ([^String s ^String needle start]
   {:pre [(string? s) (string? needle) (integer? start)]
    :post [(or (integer? %) (nil? %))]}
   (let [i (.indexOf s needle start)]
     (when-not (= i -1)
       i))))

(defn last-index-of
  "Searching backwards, return the starting position of the last occurrence of
  needle in s or nil.

  If start is provided, start the search at that position in s."
  ([^String s needle]
   {:pre [(string? s) (string? needle)]
    :post [(or (integer? %) (nil? %))]}
   (last-index-of s needle (dec (length s))))
  ([^String s ^String needle start]
   {:pre [(string? s) (string? needle) (integer? start)]
    :post [(or (integer? %) (nil? %))]}
   (let [i (.lastIndexOf s needle start)]
     (when-not (= i -1)
       i))))

(defn- slice-relative-to-end
  [s index length]
  (if (neg? (+ (superstring.core/length s) index))
    nil ; slice outside beg of s
    (slice s (+ (superstring.core/length s) index) length)))

(defn slice
  "Return a slice of s beginning at index and of the given length, or 1.

  If index is negative the starting index is relative to the end of the string.

  If the requested slice ends outside the string boundaries, we return
  the substring of s starting at index.

  Returns nil if index falls outside the string boundaries or if
  length is negative."
  ([^String s index]
   {:pre [(string? s) (integer? index)]
    :post [(or (string? %) (nil? %))]}
   (slice s index 1))
  ([^String s index length]
   {:pre [(string? s) (integer? index) (integer? length)]
    :post [(or (string? %) (nil? %))]}
   (cond
     (neg? length) nil
     (neg? (+ (superstring.core/length s) index))
     nil ; slice relative to end falls outside s
     (neg? index) (slice-relative-to-end s index length)
     (>= index (superstring.core/length s)) nil
     (> (- length index) (superstring.core/length (substring s index)))
     (substring s index)
     :else (let [end (+ index length)]
             (substring s index end)))))

(defn- equals-ignore-case [s1 s2]
  (when
      (and (= (length s1) (length s2))
           (or (= (.toLocaleLowerCase s1) (.toLocaleLowerCase s2))
               (= (.toLocaleUpperCase s1) (.toLocaleUpperCase s2))))
    s1))

(defn ends-with?
  "Return s if s ends with suffix.

  If a third argument is provided the string comparison is insensitive to case."
  ([^String s ^String suffix]
   {:pre [(string? s) (string? suffix)]
    :post [(or (string? %) (nil? %))]}
   (let [idx (index-of s suffix (- (length s) (length suffix)))]
     (when (and idx (not= idx -1))
       s)))
  ([^String s ^String suffix ignore-case]
   {:pre [(string? s) (string? suffix)]
    :post [(or (string? %) (nil? %))]}
   (if-not ignore-case
     (ends-with? s suffix)
     (let [end (substring s (max 0 (- (length s) (length suffix))))]
       (when (equals-ignore-case end suffix)
         s)))))

(defn starts-with?
  "Return s if s starts with prefix.

  If a third argument is provided the string comparison is insensitive to case."
  ([^String s ^String prefix]
   {:pre [(string? s) (string? prefix)]
    :post [(or (string? %) (nil? %))]}
   (let [idx (index-of s prefix)]
     (when (= idx 0)
       s)))
  ([^String s ^String prefix ignore-case]
   {:pre [(string? s) (string? prefix)]
    :post [(or (string? %) (nil? %))]}
   (if-not ignore-case
     (starts-with? s prefix)
     (let [beg (substring s 0 (min (length prefix) (length s)))]
       (when (equals-ignore-case beg prefix)
         s)))))

(defn chop
  "Return a new string with the last character removed.

  If the string ends with \\r\\n, both characters are removed.

  Applying chop to an empty string is a no-op.

  chomp is often a safer alternative, as it leaves the string
  unchanged if it doesn’t end in a record separator."
  [^String s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (if (ends-with? s "\r\n")
    (substring s 0 (- (length s) 2))
    (substring s 0 (max 0 (dec (length s))))))

(defn chomp
  "Return a new string with the given record separator removed from
  the end (if present).

  If separator is not provided, chomp will remove \\n, \\r or \\r\\n from
  the end of s."
  ([^String s]
   (cond
     (ends-with? s "\r\n") (substring s 0 (- (length s) 2))
     (ends-with? s "\r") (substring s 0 (dec (length s)))
     (ends-with? s "\n") (substring s 0 (dec (length s)))
     :else s))
  ([^String s ^String separator]
   (if (ends-with? s separator)
     (substring s 0 (- (length s) (length separator)))
     s)))

(defn upper-case?
  "Return s if s is all upper case.

  Characters without case, e.g. numbers, are considered to be trivially
  upper case."
  [^String s]
  {:pre [(string? s)]
   :post [(or (nil? %) (string? %))]}
  (when (reduce
         (fn [acc c] (and acc (= c (.toLocaleUpperCase c))))
         true s)
    s))

(defn lower-case?
  "Return s if s is all lower case.

  Characters without case, e.g. numbers, are considered to be trivially
  lower case."
  [^String s]
  {:pre [(string? s)]
   :post [(or (nil? %) (string? %))]}
  (when (reduce (fn [acc c] (and acc (= c (.toLocaleLowerCase c))))
                true s)
    s))

(defn swap-case
  "Change lower case characters to upper case and vice versa."
  [^String s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (let [invert-case (fn [c]
                      (cond
                        (lower-case? c) (.toLocaleUpperCase c)
                        (upper-case? c) (.toLocaleLowerCase c)
                        :else c))]
    (->> s (map invert-case) (apply str))))

(defn- gen-padding
  "Generate the necessary padding to fill s upto width."
  [s padding width]
  (let [missing (- width (length s))
        full-lengths (Math/floor (/ missing (length padding)))
        remaining (if (zero? full-lengths) (- width (length s))
                      (rem missing (* full-lengths (length padding))))]
    (str (apply str (repeat full-lengths padding))
         (substring padding 0 remaining))))


(defn pad-right
  "Pad the end of s with padding, or spaces, until the length of s matches
  width."
  ([^String s width]
   {:pre [(string? s) (integer? width)]
    :post [(string? %)]}
   (pad-right s width " "))
  ([s width padding]
   {:pre [(not-empty padding)
          (not (nil? s))]
    :post [(= (length %) width)]}
   (if (<= width (length s))
     s
     (str s (gen-padding s padding width)))))

(defn pad-left
  "Pad the beginning of s with padding, or spaces, until the length of
  s matches width."
  ([^String s width]
   {:pre [(string? s) (integer? width)]
    :post [(string? %)]}
   (pad-left s width " "))
  ([^String s width ^String padding]
   {:pre [(not-empty padding)
          (not (nil? s))]
    :post [(= (length %) width)]}
   (if (<= width (length s))
     s
     (str (gen-padding s padding width) s))))

(defn center
  "Pad both ends of s with padding, or spaces, until the length of s
  matches width."
  ([^String s width]
   {:pre [(string? s) (integer? width)]
    :post [(string? %)]}
   (center s width " "))
  ([^String s width ^String padding]
   {:pre [(not-empty padding)
          (not (nil? s))]
    :post [(= (length %) width)]}
   (if (<= width (length s))
     s
     (let [missing (- width (length s))
           full-lengths (Math/ceil (/ missing (length padding)))
           p (gen-padding s padding width)
           lengths-before (Math/floor (/ full-lengths 2))]
       (str (substring p 0 (* (length padding) lengths-before))
            s
            (substring p (* (length padding) lengths-before)))))))

(defn chop-suffix
  "If found, remove suffix from the end of s.

  Otherwise return s."
  ([^String s ^String suffix]
   {:pre [(string? s) (string? suffix)]
    :post [(string? %)]}
   (chop-suffix s suffix false))
  ([^String s ^String suffix ignore-case]
   {:pre [(string? s)
          (string? suffix)]
    :post [(string? %)]}
   (if (and (>= (length s) (length suffix))
            (ends-with? s suffix ignore-case))
     (substring s 0 (- (length s) (length suffix)))
     s)))

(defn chop-prefix
  "If found, remove prefix from the start of s.

  Otherwise return s."
  ([^String s ^String prefix]
   {:pre [(string? s) (string? prefix)]
    :post [(string? %)]}
   (chop-prefix s prefix false))
  ([^String s ^String prefix ignore-case]
   {:pre [(string? s) (string? prefix)]
    :post [(string? %)]}
   (if (and (>= (length s) (length prefix))
            (starts-with? s prefix ignore-case))
     (substring s (length prefix))
     s)))

(defn- case-sensitive-contains
  [s needle]
  (if (= needle "")
    s
    (when (index-of s needle)
      s)))

(defn re-quote
  "Return a string matching s exactly, and nothing else, for use in
  regular expressions."
  [s]
  (goog.string/regExpEscape s))

(defn- case-insensitive-contains
  [s needle]
  (if (= needle "")
    s
    (let [p (re-quote needle)]
      (when (re-find (js/RegExp. p "i") s)
        s))))

(defn contains?
  "Return s if s contains needle.

  (contains? \"foobar\" \"foo\") => \"foobar\"
  (contains? \"foobar\" \"qux\") => nil"
  ([^String s ^String needle]
   {:pre [(string? s) (string? needle)]
    :post [(or (string? %) (nil? %))]}
   (case-sensitive-contains s needle))
  ([^String s ^String needle ignore-case]
   {:pre [(string? s) (string? needle)]
    :post [(or (string? %) (nil? %))]}
   (if ignore-case
     (case-insensitive-contains s needle)
     (case-sensitive-contains s needle))))

(defn contains-all?
  "Return s if s contains all needles.

  (contains-all? \"foo bar baz\" [\"foo\" \"bar\"]) => \"foo bar baz\"
  (contains-all? \"foo bar\" [\"qux\" \"bar\"]) => nil"
  ([^String s needles]
   {:pre [(string? s) (every? string? needles)]
    :post [(or (string? %) (nil? %))]}
   (when (every? (partial case-sensitive-contains s) needles)
     s))
  ([s needles ignore-case]
   {:pre [(string? s) (every? string? needles)]
    :post [(or (string? %) (nil? %))]}
   (if ignore-case
     (when (every? (partial case-insensitive-contains s) needles)
       s)
     (contains-all? s needles))))

(defn contains-any?
  "Return s if s contains any of the needles.

  (contains-any? \"foo bar baz\" [\"foo\" \"qux\"]) => \"foo bar baz\"
  (contains-any? \"foo bar\" [\"qux\" \"quux\"]) => nil"
  ([^String s needles]
   {:pre [(string? s) (every? string? needles)]
    :post [(or (string? %) (nil? %))]}
   (some (partial case-sensitive-contains s) needles))
  ([^String s needles ignore-case]
   {:pre [(string? s) (every? string? needles)]
    :post [(or (string? %) (nil? %))]}
   (if ignore-case
     (some (partial case-insensitive-contains s) needles)
     (contains-any? s needles))))

(defn truncate
  "If s is longer than len-3, cut it down to len-3 and append '...'."
  [^String s len]
  {:pre [(string? s) (>= len 3)]
   :post [(string? s)]}
  (if (> (length s) (max 3 (- len 3)))
    (str (substring s 0 (- len 3)) "...")
    s))

(defn- char-equal-ignore-case
  [c1 c2]
  (when (or (= c1 c2)
            (= (upper-case c1) c2)
            (= (lower-case c1) c2))
    c1))

(defn common-prefix
  "Return the longest common prefix of s1 and s2.

  (common-prefix \"abadon\" \"aberdeen\") => \"ab\"
  (common-prefix \"foo\" \"bar\") => \"\""
  ([^String s1 ^String s2]
   {:pre [(string? s1) (string? s2)]
    :post [(string? %)]}
   (->> s1
        (map #(when (= %1 %2) %1) s2)
        (take-while (complement nil?))
        (apply str)))
  ([^String s1 ^String s2 ignore-case]
   {:pre [(string? s1) (string? s2)]
    :post [(string? %)]}
   (if-not ignore-case
     (common-prefix s1 s2)
     (->> s1
          (map char-equal-ignore-case s2)
          (take-while (complement nil?))
          (apply str)))))

(defn common-suffix
  "Return the longest common suffix of s1 and s2.

  (common-suffix \"bba\" \"aba\") => \"ba\"
  (common-suffix \"foo\" \"bar\") => \"\""
  ([^String s1 ^String s2]
   {:pre [(string? s1) (string? s2)]
    :post [(string? %)]}
   (->> s1 reverse (common-prefix (reverse s2)) reverse))
  ([^String s1 ^String s2 ignore-case]
   {:pre [(string? s1) (string? s2)]
    :post [(string? %)]}
   (if-not ignore-case
     (common-suffix s1 s2)
     (-> s1 reverse (common-prefix (reverse s2) :ignore-case) reverse))))

(defn wrap-words
  "Insert newlines in s so the length of each line doesn't exceed width."
  [^String s width]
  {:pre [(string? s) (integer? width)]
   :post [(string? %)]}
  (->> (split s #"\n|\t|\n| ")
       (remove empty?)
       (reduce
        (fn [{:keys [len res]} word]
          (if (< (+ len (length word)) width)
            {:len (+ len (length word) 1) :res (conj res " " word)}
            {:len (length word) :res (conj res "\n" word)}))
        {:len 0 :res []})
       :res
       rest ; drop leading " "
       (apply str)))

(defn- split-words [s]
  (remove empty?
          (-> s
              (replace #"_|-" " ")
              (replace #"([A-Z])(([A-Z])([a-z0-9]))"
                       "$1 $2")
              (replace
               #"([a-z])([A-Z])" "$1 $2")
              (split
               #"[^\w0-9]+"))))

(defn lisp-case
  "Lower case s and separate words with dashes.

  foo bar => foo-bar
  camelCase => camel-case

  This is also referred to as kebab-case in some circles."
  [^String s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (join "-" (map lower-case (split-words s))))

(defn camel-case
  "Lower case the first char in s and use capitalization to separate words.

  foo bar => fooBar
  camelCase => camelCase
  PascalCase => pascalCase"
  [^String s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (let [words (split-words s)]
    (join ""  (conj (map capitalize (rest words)) (lower-case (first words))))))

(defn pascal-case
  "Upper case the first char in s and use capitalization to separate words.

  foo bar => FooBar
  camelCase => CamelCase
  PascalCase => PascalCase"
  [^String s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (join ""  (map capitalize (split-words s))))

(defn snake-case
  "Lower case s and use underscores to separate words.

  foo bar => foo_bar
  camelCase => camel_case
  PascalCase => pascal_case"
  [^String s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (join "_"  (map lower-case (split-words s))))

(defn screaming-snake-case
  "Upper case s and use underscores to separate words.

  foo bar => FOO_BAR
  camelCase => CAMEL_CASE
  PascalCase => PASCAL_CASE"
  [^String s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (join "_"  (map upper-case (split-words s))))

(defn char-at
  "Get the character in s at index i."
  [^String s i]
  {:pre [(string? s) (integer? i) (< i (length s))]
   :post [(string? %)]}
  (.charAt s i))

(defn strip-accents
  "Strip all accents (diacritical marks) from s.

  Et ça sera sa moitié => Et ca sera sa moitie"
  [^String s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (let [from "ąàáäâãåăćčĉęèéëêĝĥìíïîĵľńňòóöőôõðśșšŝťțŭùúüűûñÿýçżźž"
        to "aaaaaaaaccceeeeeghiiiijlnnooooooossssttuuuuuunyyczzz"]
    (apply str (map (fn [c] (if-let [i (index-of from c)] (char-at to i) c)) s))))

(defn ascii?
  "Return s if s only contains ASCII characters."
  [^String s]
  {:pre [(string? s)]
   :post [(or (nil? %) (string? %))]}
  ;; The ASCII character set is encoded as the integers from 0 to 127.
  (when (reduce (fn [acc c] (and acc (< (.charCodeAt c 0) 128)))
                true s)
    s))

(defn translate
  "Translate all characters in s according to the mappings found in tmap.

  Any characters found in the set delete-chars will be pruned prior to
  consulting tmap.

  Any characters mapping to nil in tmap will also be deleted.

  (translate \"abba\" {\"a\" \"b\"}) => bbbb
  (translate \"abba\" {\"a\" \"b\", \"b\" \"a\"}) => baab
  (translate \"foo\" {\"a\" \"b\"}) =>  foo
  (translate \"gabba\" {\"a\" \"b\"} #{\"b\"}) => gbb
  (translate \"gabba\" {\\a nil} #{\\b}) => g"
  ([s tmap]
   (translate s tmap #{}))
  ([s tmap delete-chars]
   (->> s
        (remove delete-chars)
        (map (fn [c] (let [replacement (get tmap c :not-found)]
                       (cond
                         (= replacement :not-found) c
                         (nil? replacement) nil
                         :else replacement))))
        (remove nil?)
        (apply str))))

(defn slug
  "Transform s so it's suitable for use in URLs.

  The following transformations are applied:

  * Diacritical marks are removed from all characters.
  * Any character which isn't alphanumeric or in #{_-.~} is removed.
  * Lower case
  * Whitespace is collapsed and replaced by a single dash."
  [^String s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (-> s
      (replace #"\s+" "-")
      strip-accents
      (translate {"ł" "l"})
      (replace #"[^A-Za-z0-9_.~-]" "")
      (replace #"-+" "-")
      lower-case))

(defn mixed-case?
  "Return s if s contains both upper and lower case letters.

  (mixed-case? \"foo1\") => nil
  (mixed-case? \"Foo Bar\") => \"Foo Bar\""
  [^String s]
  {:pre [(string? s)]
   :post [(or (nil? %) (string? %))]}
  (let [caseless (set/intersection (set (seq (lower-case s)))
                                   (set (seq (upper-case s))))
        lower (set/difference (set (filter #(lower-case? %) s)) caseless)
        upper (set/difference (set (filter #(upper-case? %) s)) caseless)]
    (when (and (seq upper) (seq lower))
      s)))

(defn collapse-whitespace
  "Convert all adjacent whitespace in s to a single space."
  [^String s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (replace s #"[ \t\n\r]+" " "))

(defn- levenshtein-distance
  [^String s1 ^String s2]
  (let [subsolutions (atom {})
        subsolution (fn [i j]
                      (if (or (zero? i) (zero? j))
                        (max i j)
                        (get @subsolutions [i j])))]
    (doseq [i (range 1 (inc (length s1)))]
      (doseq [j (range 1 (inc (length s2)))]
        (swap! subsolutions assoc [i j]
               (min (inc (subsolution (dec i) j))
                    (inc (subsolution i (dec j)))
                    (+ (subsolution (dec i) (dec j))
                       (if (= (.charAt s1 (dec i))
                              (.charAt s2 (dec j)))
                         0
                         1))))))
    (subsolution (length s1) (length s2))))

(defn- hamming-distance [s1 s2]
  (+
   (reduce + (map #(if (= %1 %2) 0 1) s1 s2))
   (- (max (length s1) (length s2))
      (min (length s1) (length s2)))))

(defn distance
  "Get the edit distance between s1 and s2.

  The default distance metric is the Levenshtein distance.

  The optional algorithm argument can be either :levenshtein to get
  the default, or :hamming to get the Hamming distance between s1 and
  s2."
  ([^String s1 ^String s2]
   {:pre [(string? s1)
          (string? s2)]
    :post [(integer? %)]}
   (levenshtein-distance s1 s2))
  ([^String s1 ^String s2 algorithm]
   {:pre [(string? s1)
          (string? s2)]
    :post [(integer? %)]}
   (case algorithm
     :levenshtein (distance s1 s2)
     :hamming (hamming-distance s1 s2)
     (let [msg (str "Unknown algorithm: " algorithm)]
       (throw (js/Error msg))))))

(defn longest-common-substring
  "Returns the set of the longest common substrings in s1 and s2.

  This implementation uses dynamic programming, and not a generalized
  suffix tree, so the runtime is O(nm)."
  [^String s1 ^String s2]
  {:pre [(string? s1)
         (string? s2)]
   :post [(set? %)]}
  (let [rows (inc (length s1))
        cols (inc (length  s2))
        ls (let [ls (make-array rows)]
             (dotimes [row rows]
               (aset ls row (make-array cols)))
             ls)
        z (atom 0)
        ret (atom #{})]
    (doseq [i (range 0 rows)]
      (doseq [j (range 0 cols)]
        (aset ls i j 0)))
    (doseq [i (range 1 rows)]
      (doseq [j (range 1 cols)]
        (when(= (.charAt s1 (dec i)) (.charAt s2 (dec j)))
          (if (or (= i 0) (= j 0))
            (aset ls i j 1)
            (aset ls i j (inc (aget ls (dec i) (dec j)))))
          (if (> (aget ls i j) @z)
            (do
              (reset! z (aget ls i j))
              (reset! ret #{(substring s1 (- i @z) i)}))
            (when (= (aget ls i j) @z)
              (swap! ret conj (substring s1 (- i @z) i)))))))
    @ret))
