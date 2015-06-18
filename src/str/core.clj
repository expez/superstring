(ns str.core
  (:require clojure.string)
  (:refer-clojure :exclude [reverse replace contains?])
  (:import java.text.Normalizer))

(declare slice)

;; Taken from [jackknife "0.1.6"]
(defmacro ^:private defalias
  "Defines an alias for a var: a new var with the same root binding (if
  any) and similar metadata. The metadata of the alias is its initial
  metadata (as provided by def) merged into the metadata of the original."
  ([name orig]
   `(do
      (alter-meta!
       (if (.hasRoot (var ~orig))
         (def ~name (.getRawRoot (var ~orig)))
         (def ~name))
       ;; When copying metadata, disregard {:macro false}.
       ;; Workaround for http://www.assembla.com/spaces/clojure/tickets/273
       #(conj (dissoc % :macro :added)
              (apply dissoc (meta (var ~orig)) (remove #{:macro} (keys %)))))
      (var ~name)))
  ([name orig doc]
   (list `defalias (with-meta name (assoc (meta name) :doc doc)) orig)))

(defmacro ^:private alias-ns
  "Create an alias for all public vars in ns in this ns."
  [namespace]
  `(do ~@(map
          (fn [n] `(defalias ~(.sym n) ~(symbol (str (.ns n)) (str (.sym n)))))
          (vals (ns-publics namespace)))))

(alias-ns clojure.string)

(defn- ^String slice-relative-to-end
  [s index length]
  (if (neg? (+ (.length s) index)) ; slice outside beg of s
    nil
    (slice s (+ (.length s) index) length)))

(defn ^String slice
  "Return a slice of s beginning at index and of the given length.

  If index is negative the starting index is relative to the end of the string.

  The default length of the slice is 1.

  If the requested slice ends outside the string boundaries, we return
  the substring of s starting at index.

  Returns nil if index falls outside the string boundaries or if
  length is negative."
  ([^CharSequence s ^long index]
   (slice s index 1))
  ([^CharSequence s ^long index ^long length]
   (let [s (.toString s)]
     (cond
       (neg? length) nil
       (neg? (+ (.length s) index)) nil ; slice relative to end falls outside s
       (neg? index) (slice-relative-to-end s index length)
       (>= index (.length s)) nil
       (> (- length index) (.length (.substring s index))) (.substring s index)
       :else (let [end (+ index length)]
               (.substring s index end))))))

(defn ^String ends-with?
  "Return s if s ends with suffix."
  ([^CharSequence s ^CharSequence suffix]
   (let [s (.toString s)]
     (when (.endsWith s suffix)
       s)))
  ([^CharSequence s ^CharSequence suffix ignore-case]
   (if-not ignore-case
     (ends-with? s suffix)
     (let [s (.toString s)
           suffix (.toString suffix)
           end (.substring s (max 0 (- (.length s) (.length suffix))))]
       (when (.equalsIgnoreCase end suffix)
         s)))))

(defn ^String starts-with?
  "Return s if s starts with with prefix.

  If a third argument is provided the string comparison is insensitive to case."
  ([^CharSequence s ^CharSequence prefix]
   (when (.startsWith s prefix)
     s))
  ([^CharSequence s ^CharSequence prefix ignore-case]
   (if-not ignore-case
     (starts-with? s prefix)
     (let [s (.toString s)
           prefix (.toString prefix)
           beg (.substring s 0 (.length prefix))]
       (when (.equalsIgnoreCase beg prefix)
         s)))))

(defn ^String chop
  "Return a new string with the last character removed.

  If the string ends with \\r\\n, both characters are removed.

  Applying chop to an empty string is a no-op."
  [^CharSequence s]
  (let [s (.toString s)]
    (if (.endsWith s "\r\n")
      (.substring s 0 (- (.length s) 2))
      (.substring s 0 (max 0 (dec (.length s)))))))

(defn ^String chomp
  "Return a new string with the given record separator removed from
  the end (if present).

  If separator is not provided chomp will remove \\n, \\r or \\r\\n from
  the end of s."
  ([^CharSequence s]
   (let [s (.toString s)]
     (cond
       (.endsWith s "\r\n") (.substring s 0 (- (.length s) 2))
       (.endsWith s "\r") (.substring s 0 (dec (.length s)))
       (.endsWith s "\n") (.substring s 0 (dec (.length s)))
       :else s)))
  ([^CharSequence s ^CharSequence separator]
   (let [s (.toString s)]
     (if (.endsWith s separator)
       (.substring s 0 (- (.length s) (.length separator)))
       s))))

(defn ^String capitalize
  "Return a new string where the first character is in upper case and
  all others in lower case."
  [^CharSequence s]
  (let [s (.toString s)]
    (case (.length s)
      0 ""
      1 (upper-case s)
      (str (upper-case (.substring s 0 1)) (lower-case (.substring s 1))))) )

(defn ^String swap-case
  "Change lower case characters to upper case and vice versa."
  [^CharSequence s]
  (let [invert-case (fn [c]
                      (cond
                        (Character/isLowerCase c) (Character/toUpperCase c)
                        (Character/isUpperCase c) (Character/toLowerCase c)
                        :else c))]
    (->> s .toString (map invert-case) (apply str))))

(defn- gen-padding
  "Generate the necessary padding to fill s upto width."
  [^CharSequence s ^CharSequence padding ^Long width]
  (let [missing (- width (.length s))
        full-lengths (Math/floor (/ missing (.length padding)))
        remaining (if (zero? full-lengths) (- width (.length s))
                      (rem missing (* full-lengths (.length padding))))
        s (.toString s)
        padding (.toString padding)]
    (.concat (apply str (repeat full-lengths padding))
             (.substring padding 0 remaining))))


(defn ^String pad-right
  "Pad the end of s with padding, or spaces, until the length of s matches
  width."
  ([^CharSequence s ^long width]
   (pad-right s width " "))
  ([^CharSequence s ^Long width ^CharSequence padding]
   {:pre [(not-empty padding)
          (not (nil? s))]
    :post [(= (.length %) width)]}
   (let [s (.toString s)]
     (if (<= width (.length s))
       s
       (.concat s (gen-padding s padding width))))))

(defn ^String pad-left
  "Pad the beginning of s with padding, or spaces, until the length of
  s matches width."
  ([^CharSequence s ^Long width]
   (pad-left s width " "))
  ([^CharSequence s ^Long width ^CharSequence padding]
   {:pre [(not-empty padding)
          (not (nil? s))]
    :post [(= (.length %) width)]}
   (let [s (.toString s)]
     (if (<= width (.length s))
       s
       (.concat (gen-padding s padding width) s)))))

(defn ^String center
  "Pad both ends of s with padding, or spaces, until the length of s
  matches width."
  ([^CharSequence s ^Long width]
   (center s width " "))
  ([^CharSequence s ^Long width ^CharSequence padding]
   {:pre [(not-empty padding)
          (not (nil? s))]
    :post [(= (.length %) width)]}
   (let [s (.toString s)
         padding (.toString padding)]
     (if (<= width (.length s))
       s
       (let [missing (- width (.length s))
             full-lengths (Math/ceil (/ missing (.length padding)))
             p (gen-padding s padding width)
             lengths-before (Math/floor (/ full-lengths 2))]
         (str (.substring p 0 (* (.length padding) lengths-before))
              s
              (.substring p (* (.length padding) lengths-before))))))))

(defn ^String chop-suffix
  "If s ends with suffix return a new string without the suffix.

  Otherwise return s."
  ([^CharSequence s ^CharSequence suffix]
   (chop-suffix s suffix false))
  ([^CharSequence s ^CharSequence suffix ignore-case]
   {:pre [(not (nil? s))
          (not (nil? suffix))]
    :post [(not (nil? %))]}
   (let [s (.toString s)
         suffix (.toString suffix)]
     (if (and (>= (.length s) (.length suffix))
              (ends-with? s suffix ignore-case))
       (.substring s 0 (- (.length s) (.length suffix)))
       s))))

(defn ^String chop-prefix
  "If s starts with with prefix return a new string without the prefix.

  Otherwise return s."
  ([^CharSequence s ^CharSequence prefix]
   (chop-prefix s prefix false))
  ([^CharSequence s ^CharSequence prefix ignore-case]
   {:pre [(not (nil? s))
          (not (nil? prefix))]
    :post [(not (nil? %))]}
   (let [s (.toString s)
         prefix (.toString prefix)]
     (if (and (>= (.length s) (.length prefix))
              (starts-with? s prefix ignore-case))
       (.substring s (.length prefix))
       s))))

(defn- case-sensitive-contains
  [s needle]
  (if (= needle "")
    s
    (when (and (seq s) (seq needle) (.contains s needle))
      s)))

(defn- case-insensitive-contains
  [s needle]
  (if (= needle "")
    s
    (when (and (seq s) (seq needle))
      (let [p (java.util.regex.Pattern/compile
               (java.util.regex.Pattern/quote needle)
               (bit-or java.util.regex.Pattern/CASE_INSENSITIVE
                       java.util.regex.Pattern/UNICODE_CASE))]
        (when (re-find p s)
          s)))))

(defn ^CharSequence contains?
  "Return s if s contains needle."
  ([^CharSequence s ^CharSequence needle]
   {:pre [(not (nil? s))
          (not (nil? needle))]
    :post [(or (nil? %) (string? %))]}
   (case-sensitive-contains (.toString s) (.toString needle)))
  ([^CharSequence s ^CharSequence needle ignore-case]
   {:pre [(not (nil? s))
          (not (nil? needle))]
    :post [(or (nil? %) (string? %))]}
   (if ignore-case
     (case-insensitive-contains (.toString s) (.toString needle))
     (case-sensitive-contains (.toString s) (.toString needle)))))

(defn ^String contains-all?
  "Return s if s contains all needles."
  ([^CharSequence s needles]
   (let [s (.toString s)
         needles (map #(.toString %) needles)]
     (when (every? (partial case-sensitive-contains s) needles)
       s)))
  ([^CharSequence s needles ignore-case]
   (let [s (.toString s)
         needles (map #(.toString %) needles)]
     (if ignore-case
       (when (every? (partial case-insensitive-contains s) needles)
         s)
       (contains-all? s needles)))))

(defn ^String contains-any?
  "Return s if s contains any of the needles."
  ([^CharSequence s needles]
   (let [s (.toString s)
         needles (map #(.toString %) needles)]
     (some (partial case-sensitive-contains s) needles)))
  ([^CharSequence s needles ignore-case]
   (let [s (.toString s)
         needles (map #(.toString %) needles)]
     (if ignore-case
       (some (partial case-insensitive-contains s) needles)
       (contains-any? s needles)))))

(defn ^String truncate
  "If s is longer than len-3, cut it down to len-3 and append '...'."
  [^CharSequence s len]
  {:pre [(not (nil? s)) (>= len 3)]}
  (let [s (.toString s)]
    (if (> (.length s) (max 3 (- len 3)))
      (str (.substring s 0 (- len 3)) "...")
      s)))

(defn ^String common-prefix
  "Return the longest common prefix of s1 and s2."
  ([^CharSequence s1 ^CharSequence s2]
   {:pre [(not (nil? s1)) (not (nil? s2))]
    :post [(string? %)]}
   (->> s1
        .toString
        (map #(when (= %1 %2) %1) (.toString s2))
        (take-while (complement nil?))
        (apply str)))
  ([^CharSequence s1 ^CharSequence s2 ignore-case]
   {:pre [(not (nil? s1)) (not (nil? s2))]
    :post [(string? %)]}
   (if-not ignore-case
     (common-prefix s1 s2)
     (->> s1
          .toString
          (map #(when (or (= %1 %2)
                          (= (Character/toUpperCase %1) %2)
                          (= (Character/toLowerCase %1) %2))
                  %1)
               (.toString s2))
          (take-while (complement nil?))
          (apply str)))))

(defn ^String common-suffix
  "Return the longest common suffix of s1 and s2."
  ([^CharSequence s1 ^CharSequence s2]
   {:pre [(not (nil? s1)) (not (nil? s2))]
    :post [(string? %)]}
   (->> s1
        reverse
        (map #(when (= %1 %2) %1) (reverse s2))
        (take-while (complement nil?))
        (apply str)
        reverse))
  ([^CharSequence s1 ^CharSequence s2 ignore-case]
   {:pre [(not (nil? s1)) (not (nil? s2))]
    :post [(string? %)]}
   (if-not ignore-case
     (common-suffix s1 s2)
     (->> s1
          reverse
          (map #(when (or (= %1 %2)
                          (= (Character/toUpperCase %1) %2)
                          (= (Character/toLowerCase %1) %2))
                  %1)
               (reverse s2))
          (take-while (complement nil?))
          (apply str)
          reverse))))

(defn- upper-exists?
  "Does c exist in an upper case version?"
  [^Character c]
  (or (Character/isUpperCase c) (not= c (Character/toUpperCase c))))

(defn ^String title-case
  "Capitalize the first character of s and lower case the rest."
  [^CharSequence s]
  {:pre [(not (nil? s))]
   :post [(string? %)]}
  (let [s (.toString s)
        first-char (first s)
        start (if (upper-exists? first-char) (upper-case first-char) first-char)]
    (str start (lower-case (.substring s 1)))))

(defn ^String upper-case?
  "Return s if s is all upper case.

  Characters without case, e.g. numbers, are considered to be trivially
  upper case."
  [^CharSequence s]
  {:pre [(not (nil? s))]
   :post [(or (nil? %) (string? %))]}
  (when (reduce
         (fn [acc c]
           (and acc (or (Character/isUpperCase c) (= c (Character/toUpperCase c)))))
         true (.toString s))
    s))

(defn ^String lower-case?
  "Return s if s is all lower case.

  Characters without case, e.g. numbers, are considered to be trivially
  lower case."
  [^CharSequence s]
  {:pre [(not (nil? s))]
   :post [(or (nil? %) (string? %))]}
  (when (reduce
         (fn [acc c]
           (and acc (or (Character/isLowerCase c) (= c (Character/toLowerCase c)))))
         true (.toString s))
    s))

(defn- split-words [^CharSequence s]
  (remove empty?
          (-> s
              (replace #"_|-" " ")
              (replace #"(\p{javaUpperCase})((\p{javaUpperCase})[(\p{javaLowerCase})0-9])"
                       "$1 $2")
              (replace
               #"(\p{javaLowerCase})(\p{javaUpperCase})" "$1 $2")
              (split
               #"[^\w0-9]+"))))

(defn wrap-words
  "Insert newlines in s so the length of each line doesn't exceed width."
  [^CharSequence s width]
  (->> (split s #"\n|\t|\n| ")
       (remove empty?)
       (reduce
        (fn [{:keys [len res]} word]
          (if (< (+ len (.length word)) width)
            {:len (+ len (.length word) 1) :res (conj res " " word)}
            {:len (.length word) :res (conj res "\n" word)}))
        {:len 0 :res []})
       :res
       rest ; drop leading " "
       (apply str)))

(defn ^String lisp-case
  "Lower case s and separate words with dashes.

  foo bar => foo-bar
  camelCase => camel-case

  This is also referred to as kebab-case in some circles."
  [^CharSequence s]
  {:pre [(not (nil? s))]
   :post [(string? %)]}
  (join "-" (map lower-case (split-words s))))

(defn ^String camel-case
  "Lower case first char in s and use capitalization to separate words.

  foo bar => fooBar
  camelCase => camelCase
  PascalCase => pascalCase"
  [^CharSequence s]
  {:pre [(not (nil? s))]
   :post [(string? %)]}
  (let [words (split-words s)]
    (join ""  (conj (map capitalize (rest words)) (lower-case (first words))))))

(defn ^String pascal-case
  "Lower case first char in s and use capitalization to separate words.

  foo bar => FooBar
  camelCase => CamelCase
  PascalCase => PascalCase"
  [^CharSequence s]
  {:pre [(not (nil? s))]
   :post [(string? %)]}
  (join ""  (map capitalize (split-words s))))

(defn ^String snake-case
  "Lower case s and use underscores to separate words.

  foo bar => foo_bar
  camelCase => camel_case
  PascalCase => pascal_case"
  [^CharSequence s]
  {:pre [(not (nil? s))]
   :post [(string? %)]}
  (join "_"  (map lower-case (split-words s))))

(defn ^String screaming-snake-case
  "Upper case s and use underscores to separate words.

  foo bar => FOO_BAR
  camelCase => CAMEL_CASE
  PascalCase => PASCAL_CASE"
  [^CharSequence s]
  {:pre [(not (nil? s))]
   :post [(string? %)]}
  (join "_"  (map upper-case (split-words s))))

(defn ^String strip-accents
  "Strip all accents (diacritical marks) from s.

  Et ça sera sa moitié => Et ca sera sa moitie"
  [^CharSequence s]
  {:pre [(not (nil? s))]
   :post [(string? %)]}
  (-> s
      .toString
      (Normalizer/normalize java.text.Normalizer$Form/NFD)
      (.replaceAll  "\\p{InCombiningDiacriticalMarks}+" "")))

(defn ^String ascii?
  "Return s if s only contains ASCII characters."
  [^CharSequence s]
  {:pre [(not (nil? s))]
   :post [(or (nil? %) (string? %))]}
  ;; The ASCII character set is encoded as the integers from 0 to 127.
  (when (reduce (fn [acc c] (and acc (< (int c) 128))) true (.toString s))
    s))

(defn ^String slug
  "Transform s so it's suitable for use in URLs.

  The following transformations are applied:

  * Diacritical marks are removed from all characters.
  * Any character which isn't alphanumeric or in #{_-.~} is removed.
  * Lower case
  * Whitespace is collapsed and replaced by a single dash."
  [^CharSequence s]
  {:pre [(not (nil? s))]
   :post [(string? %)]}
  (-> s
      .toString
      (.replaceAll "\\s+" "-")
      strip-accents
      (.replaceAll "[^A-Za-z0-9_.~-]" "")
      (.replaceAll "-+" "-")
      lower-case))

(defn- upper-if-upper-exists?
  "Is c uppercase for those characters which have an upper case version?"
  [^Character c]
  (or (Character/isUpperCase c) (= c (Character/toUpperCase c))))

(defn- lower-if-lower-exists?
  "Is c lower for those characters which have an lower case version?"
  [^Character c]
  (or (Character/isLowerCase c) (= c (Character/toLowerCase c))))

(defn ^String mixed-case?
  "Return s if s contains both upper and lower case letters. "
  [^CharSequence s]
  {:pre [(not (nil? s))]
   :post [(or (nil? %) (string? %))]}
  (let [s (.toString s)]
    (when (and (seq (drop-while (complement upper-if-upper-exists?) s))
               (seq (drop-while (complement lower-if-lower-exists?) s)))
      s)))

(defn ^String collapse-whitespace
  "Convert all adjacent whitespace characters in s to a single space."
  [^CharSequence s]
  {:pre [(not (nil? s))]
   :post [(string? %)]}
  (.replaceAll (.toString s) "[ \t\n\r]+" " "))

(defn- ^Long levenshtein-distance
  [s1 s2]
  (let [subsolutions (atom {})
        subsolution (fn [i j]
                      (if (or (zero? i) (zero? j))
                        (max i j)
                        (get @subsolutions [i j])))]
    (doseq [i (range 1 (inc (.length s1)))]
      (doseq [j (range 1 (inc (.length s2)))]
        (swap! subsolutions assoc [i j]
               (min (inc (subsolution (dec i) j))
                    (inc (subsolution i (dec j)))
                    (+ (subsolution (dec i) (dec j))
                       (if (= (.charAt s1 (dec i))
                              (.charAt s2 (dec j)))
                         0
                         1))))))
    (subsolution (.length s1) (.length s2))))

(defn- ^Long hamming-distance [s1 s2]
  (+
   (reduce + (map #(if (= %1 %2) 0 1) s1 s2))
   (- (max (.length s1) (.length s2))
      (min (.length s1) (.length s2)))))

(defn ^Long distance
  "Get the distance between s1 and s2.

  The default distance metric is the Lehvenstein distance.

  The optional algorithm argument can be either :levenshtein to get
  the default, or :hamming to get the Hamming distance between s1 and
  s2."
  ([^CharSequence s1 ^CharSequence s2]
   {:pre [(not (nil? s1))
          (not (nil? s2))]
    :post [(integer? %)]}
   (levenshtein-distance (.toString s1) (.toString s2)))
  ([^CharSequence s1 ^CharSequence s2 algorithm]
   {:pre [(not (nil? s1))
          (not (nil? s2))]
    :post [(integer? %)]}
   (let [s1 (.toString s1)
         s2 (.toString s2)]
     (case algorithm
       :levenshtein (distance s1 s2)
       :hamming (hamming-distance s1 s2)
       (throw (IllegalArgumentException. (str "Unknown algorithm: " algorithm)))))))

(defn longest-common-substrings
  "Returns the set of the longest common substrings in s1 and s2.

  This implementation uses dynamic programming, and not a generalized
  suffix tree, so the runtime is O(nm)."
  [^CharSequence s1 ^CharSequence s2]
  {:pre [(not (nil? s1))
         (not (nil? s2))]
   :post [(set? %)]}
  (let [s1 (.toString s1)
        s2 (.toString s2)
        rows (inc (.length s1))
        cols (inc (.length  s2))
        ls (make-array Long rows cols)
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
              (reset! ret #{(.substring s1 (- i @z) i)}))
            (when (= (aget ls i j) @z)
              (swap! ret conj (.substring s1 (- i @z) i)))))))
    @ret))
