(ns superstring.core
  (:require clojure.string)
  (:refer-clojure :exclude [reverse replace])
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
       #(conj (dissoc % :macro)
              (apply dissoc (dissoc (meta (var ~orig)) :added)
                     (remove #{:macro} (keys %)))))
      (var ~name)))
  ([name orig doc]
   (list `defalias (with-meta name (assoc (meta name) :doc doc)) orig)))

(defmacro ^:private alias-ns
  "Create an alias for all public vars in ns in this ns."
  [namespace]
  `(do ~@(map
          (fn [^clojure.lang.Var n] `(defalias ~(.sym n) ~(symbol (str (.ns n)) (str (.sym n)))))
          (vals (ns-publics namespace)))))

(alias-ns clojure.string)
(defalias substring clojure.core/subs)

(defn length
  "Return the length of s."
  ^long [^String s]
  {:pre [(string? s)]
   :post [(integer? %)]}
  (.length s))

(defn- slice-relative-to-end
  [s index length]
  (if (neg? (+ (superstring.core/length s) index))
    nil ; slice outside beg of s
    (slice s (+ (superstring.core/length s) index) length)))

(defn slice
  "Return a slice of s beginning at index and of the given length, or 1.

  If index is negative, the starting index is relative to the end of the string.

  If the requested slice ends outside the string boundaries, we return
  the substring of s starting at index.

  Returns nil if index falls outside the string boundaries or if
  length is negative."
  (^String
   [^String s ^long index]
   {:pre [(string? s) (integer? index)]
    :post [(or (string? %) (nil? %))]}
   (slice s index 1))
  (^String
   [^String s ^long index ^long length]
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

(defn ends-with?
  "Return s if s ends with suffix.

  If a third argument is provided the string comparison is insensitive to case."
  (^String
   [^CharSequence s ^String suffix]
   {:pre [(string? suffix)]
    :post [(or (string? %) (nil? %))]}
   (let [s (.toString s)]
     (when (.endsWith s suffix)
       s)))
  (^String
   [^CharSequence s ^String suffix ignore-case]
   {:pre [(string? suffix)]
    :post [(or (string? %) (nil? %))]}
   (if-not ignore-case
     (ends-with? s suffix)
     (let [s (.toString s)
           end (substring s (max 0 (- (length s) (length suffix))))]
       (when (.equalsIgnoreCase end suffix)
         s)))))

(defn ^String starts-with?
  "Return s if s starts with prefix.

  If a third argument is provided the string comparison is insensitive to case."
  (^String
   [^CharSequence s ^String prefix]
   {:pre [(string? prefix)]
    :post [(or (string? %) (nil? %))]}
   (let [s (.toString s)]
     (when (.startsWith s prefix)
       s)))
  (^String
   [^CharSequence s ^String prefix ignore-case]
   {:pre [(string? prefix)]
    :post [(or (string? %) (nil? %))]}
   (if-not ignore-case
     (starts-with? s prefix)
     (let [s (.toString s)
           beg (substring s 0 (min (length s) (length prefix)))]
       (when (.equalsIgnoreCase beg prefix)
         s)))))

(defn chop
  "Return a new string with the last character removed.

  If the string ends with \\r\\n, both characters are removed.

  Applying chop to an empty string is a no-op.

  chomp is often a safer alternative, as it leaves the string
  unchanged if it doesn’t end in a record separator."
  ^String [^String s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (if (.endsWith s "\r\n")
    (substring s 0 (- (length s) 2))
    (substring s 0 (max 0 (dec (length s))))))

(defn chomp
  "Return a new string with the given record separator removed from
  the end (if present).

  If separator is not provided, chomp will remove \\n, \\r or \\r\\n from
  the end of s."
  (^String
   [^String s]
   {:pre [(string? s)]
    :post [(string? %)]}
   (cond
     (.endsWith s "\r\n") (substring s 0 (- (length s) 2))
     (.endsWith s "\r") (substring s 0 (dec (length s)))
     (.endsWith s "\n") (substring s 0 (dec (length s)))
     :else s))
  (^String
   [^String s ^String separator]
   {:pre [(string? s) (string? separator)]
    :post [(string? %)]}
   (if (.endsWith s separator)
     (substring s 0 (- (length s) (length separator)))
     s)))

(defn ^String upper-case?
  "Return s if s is all upper case.

  Characters without case, e.g. numbers, are considered to be trivially
  upper case."
  ^String [^String s]
  {:pre [(string? s)]
   :post [(or (nil? %) (string? %))]}
  (when (reduce
         (fn [acc ^Character c] (and acc (= c (Character/toUpperCase c))))
         true s)
    s))

(defn lower-case?
  "Return s if s is all lower case.

  Characters without case, e.g. numbers, are considered to be trivially
  lower case."
  ^String [^String s]
  {:pre [(string? s)]
   :post [(or (nil? %) (string? %))]}
  (when (reduce (fn [acc ^Character c] (and acc (= c (Character/toLowerCase c))))
                true s)
    s))

(defn swap-case
  "Change lower case characters to upper case and vice versa."
  ^String [^String s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (let [invert-case (fn [^Character c]
                      (cond
                        (Character/isUpperCase c) (Character/toLowerCase c)
                        (= c \ß) "SS" ; this uppers to itself
                        (Character/isLowerCase c) (Character/toUpperCase c)
                        :else c))]
    (->> s (map invert-case) (apply str))))

(defn- gen-padding
  "Generate the necessary padding to fill s upto width."
  ^String [^String s ^String padding ^long width]
  (let [missing (- width (length s))
        full-lengths (Math/floor (/ missing (length padding)))
        remaining (if (zero? full-lengths) (- width (length s))
                      (rem missing (* full-lengths (length padding))))]
    (str (apply str (repeat full-lengths padding))
         (substring padding 0 remaining))))


(defn pad-right
  "Pad the end of s with padding, or spaces, until the length of s matches
  width."
  (^String
   [^String s ^long width]
   {:pre [(string? s) (integer? width)]
    :post [(string? %)]}
   (pad-right s width " "))
  (^String
   [^String s ^long width ^String padding]
   {:pre [(not-empty padding)
          (not (nil? s))]
    :post [(= (length %) width)]}
   (if (<= width (length s))
     s
     (str s (gen-padding s padding width)))))

(defn pad-left
  "Pad the beginning of s with padding, or spaces, until the length of
  s matches width."
  (^String
   [^String s ^long width]
   {:pre [(string? s) (integer? width)]
    :post [(string? %)]}
   (pad-left s width " "))
  (^String
   [^String s ^long width ^String padding]
   {:pre [(not-empty padding)
          (not (nil? s))]
    :post [(= (length %) width)]}
   (if (<= width (length s))
     s
     (str (gen-padding s padding width) s))))

(defn center
  "Pad both ends of s with padding, or spaces, until the length of s
  matches width."
  (^String
   [^String s width]
   {:pre [(string? s) (integer? width)]
    :post [(string? %)]}
   (center s width " "))
  (^String
   [^String s width ^String padding]
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
  (^String
   [^String s ^String suffix]
   {:pre [(string? s) (string? suffix)]
    :post [(string? %)]}
   (chop-suffix s suffix false))
  (^String
   [^String s ^String suffix ignore-case]
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
  (^String
   [^String s ^String prefix]
   {:pre [(string? s) (string? prefix)]
    :post [(string? %)]}
   (chop-prefix s prefix false))
  (^String
   [^String s ^String prefix ignore-case]
   {:pre [(string? s) (string? prefix)]
    :post [(string? %)]}
   (if (and (>= (length s) (length prefix))
            (starts-with? s prefix ignore-case))
     (substring s (length prefix))
     s)))

(defn- case-sensitive-contains
  ^String [^String s ^String needle]
  (if (= needle "")
    s
    (when (.contains s needle)
      s)))

(defn- case-insensitive-contains
  [s needle]
  (if (= needle "")
    s
    (let [p (java.util.regex.Pattern/compile
             (java.util.regex.Pattern/quote needle)
             (bit-or java.util.regex.Pattern/CASE_INSENSITIVE
                     java.util.regex.Pattern/UNICODE_CASE))]
      (when (re-find p s)
        s))))

(defn includes?
  "Return s if s includes needle.

  (includes? \"foobar\" \"foo\") => \"foobar\"
  (includes? \"foobar\" \"qux\") => nil"
  (^String
   [^CharSequence s ^String needle]
   {:pre [(string? needle)]
    :post [(or (string? %) (nil? %))]}
   (case-sensitive-contains (.toString s) needle))
  (^String
   [^CharSequence s ^String needle ignore-case]
   {:pre [(string? needle)]
    :post [(or (string? %) (nil? %))]}
   (let [s (.toString s)]
     (if ignore-case
       (case-insensitive-contains s needle)
       (case-sensitive-contains s needle)))))

(defn includes-all?
  "Return s if s includes all needles.

  (includes-all? \"foo bar baz\" [\"foo\" \"bar\"]) => \"foo bar baz\"
  (includes-all? \"foo bar\" [\"qux\" \"bar\"]) => nil"
  (^String
   [^String s needles]
   {:pre [(string? s) (every? string? needles)]
    :post [(or (string? %) (nil? %))]}
   (when (every? (partial case-sensitive-contains s) needles)
     s))
  (^String
   [^String s needles ignore-case]
   {:pre [(string? s) (every? string? needles)]
    :post [(or (string? %) (nil? %))]}
   (if ignore-case
     (when (every? (partial case-insensitive-contains s) needles)
       s)
     (includes-all? s needles))))

(defn includes-any?
  "Return s if s includes any of the needles.

  (includes-any? \"foo bar baz\" [\"foo\" \"qux\"]) => \"foo bar baz\"
  (includes-any? \"foo bar\" [\"qux\" \"quux\"]) => nil"
  (^String
   [^String s needles]
   {:pre [(string? s) (every? string? needles)]
    :post [(or (string? %) (nil? %))]}
   (some (partial case-sensitive-contains s) needles))
  (^String
   [^String s needles ignore-case]
   {:pre [(string? s) (every? string? needles)]
    :post [(or (string? %) (nil? %))]}
   (if ignore-case
     (some (partial case-insensitive-contains s) needles)
     (includes-any? s needles))))

(defn truncate
  "If s is longer than len-3, cut it down to len-3 and append '...'."
  ^String [^String s len]
  {:pre [(string? s) (>= len 3)]
   :post [(string? s)]}
  (if (> (length s) (max 3 (- len 3)))
    (str (substring s 0 (- len 3)) "...")
    s))

(defn- char-equal-ignore-case
  [^Character c1 ^Character c2]
  (when (or (= c1 c2)
            (= (Character/toUpperCase c1) c2)
            (= (Character/toLowerCase c1) c2))
    c1))

(defn common-prefix
  "Return the longest common prefix of s1 and s2.

  (common-prefix \"abadon\" \"aberdeen\") => \"ab\"
  (common-prefix \"foo\" \"bar\") => \"\""
  (^String
   [^String s1 ^String s2]
   {:pre [(string? s1) (string? s2)]
    :post [(string? %)]}
   (->> s1
        (map #(when (= %1 %2) %1) s2)
        (take-while (complement nil?))
        (apply str)))
  (^String
   [^String s1 ^String s2 ignore-case]
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
  (^String
   [^String s1 ^String s2]
   {:pre [(string? s1) (string? s2)]
    :post [(string? %)]}
   (->> s1 reverse (common-prefix (reverse s2)) reverse))
  (^String
   [^String s1 ^String s2 ignore-case]
   {:pre [(string? s1) (string? s2)]
    :post [(string? %)]}
   (if-not ignore-case
     (common-suffix s1 s2)
     (-> s1 reverse (common-prefix (reverse s2) :ignore-case) reverse))))

(defn wrap-words
  "Insert newlines in s so the length of each line doesn't exceed width."
  ^String [^String s ^long width]
  {:pre [(string? s) (integer? width)]
   :post [(string? %)]}
  (->> (split s #"\n|\t|\n| ")
       (remove empty?)
       (reduce
        (fn [{:keys [len res]} ^String word]
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
              (replace #"(\p{javaUpperCase})((\p{javaUpperCase})[(\p{javaLowerCase})0-9])"
                       "$1 $2")
              (replace
               #"(\p{javaLowerCase})(\p{javaUpperCase})" "$1 $2")
              (split
               #"[^\w0-9]+"))))

(defn lisp-case
  "Lower case s and separate words with dashes.

  foo bar => foo-bar
  camelCase => camel-case

  This is also referred to as kebab-case in some circles."
  ^String [^String s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (join "-" (map lower-case (split-words s))))

(defn camel-case
  "Lower case the first char in s and use capitalization to separate words.

  foo bar => fooBar
  camelCase => camelCase
  PascalCase => pascalCase"
  ^String [^String s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (let [words (split-words s)]
    (join ""  (conj (map capitalize (rest words)) (lower-case (first words))))))

(defn pascal-case
  "Upper the case first char in s and use capitalization to separate words.

  foo bar => FooBar
  camelCase => CamelCase
  PascalCase => PascalCase"
  ^String [^String s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (join ""  (map capitalize (split-words s))))

(defn snake-case
  "Lower case s and use underscores to separate words.

  foo bar => foo_bar
  camelCase => camel_case
  PascalCase => pascal_case"
  ^String [^String s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (join "_"  (map lower-case (split-words s))))

(defn screaming-snake-case
  "Upper case s and use underscores to separate words.

  foo bar => FOO_BAR
  camelCase => CAMEL_CASE
  PascalCase => PASCAL_CASE"
  ^String [^String s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (join "_"  (map upper-case (split-words s))))

(defn strip-accents
  "Strip all accents (diacritical marks) from s.

  Et ça sera sa moitié => Et ca sera sa moitie"
  ^String [^String s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (-> s
      (Normalizer/normalize java.text.Normalizer$Form/NFD)
      (.replaceAll  "\\p{InCombiningDiacriticalMarks}+" "")))

(defn ascii?
  "Return s if s only contains ASCII characters."
  ^String [^String s]
  {:pre [(string? s)]
   :post [(or (nil? %) (string? %))]}
  ;; The ASCII character set is encoded as the integers from 0 to 127.
  (when (reduce (fn [acc c] (and acc (< (int c) 128)))
                true s)
    s))

(defn translate
  "Translate all characters in s according to the mappings found in tmap.

  Any characters found in the set delete-chars will be pruned prior to
  consulting tmap.

  Any characters mapping to nil in tmap will also be deleted.

  (translate \"abba\" {\\a \\b}) => bbbb
  (translate \"abba\" {\\a \\b, \\b \\a}) => baab
  (translate \"foo\" {\\a \b}) =>  foo
  (translate \"gabba\" {\\a \\b} #{\\b}) => gbb
  (translate \"gabba\" {\\a nil} #{\\b}) => g"
  (^String
   [^String s ^clojure.lang.APersistentMap tmap]
   {:pre [(string? s) (map? tmap)]
    :post [(string? %)]}
   (translate s tmap #{}))
  (^String
   [^String s
    ^clojure.lang.APersistentMap tmap
    ^clojure.lang.APersistentSet delete-chars]
   {:pre [(string? s) (map? tmap) (set? delete-chars)]
    :post [(string? %)]}
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
  ^String [^String s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (-> s
      (replace #"\s+" "-")
      strip-accents
      (translate {\ł \l})
      (replace #"[^A-Za-z0-9_.~-]" "")
      (replace #"-+" "-")
      lower-case))

(defn mixed-case?
  "Return s if s contains both upper and lower case letters.

  (mixed-case? \"foo1\") => nil
  (mixed-case? \"Foo Bar\") => \"Foo Bar\""
  ^String [^String s]
  {:pre [(string? s)]
   :post [(or (nil? %) (string? %))]}
  (when (and (seq (filter #(Character/isLowerCase ^Character %) s))
             (seq (filter #(Character/isUpperCase ^Character %) s)))
    s))

(defn collapse-whitespace
  "Convert all adjacent whitespace in s to a single space."
  ^String [^String s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (replace s #"[ \t\n\r]+" " "))

(defn- levenshtein-distance
  ^long [^String s1 ^String s2]
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
  (^long
   [^String s1 ^String s2]
   {:pre [(string? s1)
          (string? s2)]
    :post [(integer? %)]}
   (levenshtein-distance s1 s2))
  (^long
   [^String s1 ^String s2 algorithm]
   {:pre [(string? s1)
          (string? s2)]
    :post [(integer? %)]}
   (case algorithm
     :levenshtein (distance s1 s2)
     :hamming (hamming-distance s1 s2)
     (let [msg (str "Unknown algorithm: " algorithm)]
       (throw (IllegalArgumentException. msg))))))

(defn longest-common-substring
  "Returns the set of the longest common substrings in s1 and s2.

  This implementation uses dynamic programming, and not a generalized
  suffix tree, so the runtime is O(nm)."
  ^String [^String s1 ^String s2]
  {:pre [(string? s1)
         (string? s2)]
   :post [(set? %)]}
  (let [rows (inc (length s1))
        cols (inc (length  s2))
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
              (reset! ret #{(substring s1 (- i @z) i)}))
            (when (= (aget ls i j) @z)
              (swap! ret conj (substring s1 (- i @z) i)))))))
    @ret))

(defn char-at
  "Get the character in s at index i."
  {:added "1.1"}
  ^Character [^String s ^long i]
  {:pre [(string? s) (integer? i) (< i (length s))]
   :post (instance? Character %)}
  (.charAt s i))

(defn re-quote
  "Create a string matching s exactly, and nothing else, for use in
  regular expressions."
  {:added "1.1"}
  ^String [^String s]
  {:pre [(string? s)]
   :post (string? %)}
  (java.util.regex.Pattern/quote s))

; URL encoding functions taken from https://github.com/slipstream/SlipStreamUI/blob/master/clj/src/slipstream/ui/util/clojure.clj
; DOC: https://tools.ietf.org/html/rfc3986
; SOURCE: http://www.w3schools.com/tags/ref_urlencode.asp
; SOURCE: http://www.degraeve.com/reference/urlencoding.php
; SOURCE: https://en.wikipedia.org/wiki/Percent-encoding#Percent-encoding_reserved_characters

(def ^:private ^:const url-encode-chars-strict
  "URL reserved characters as per https://tools.ietf.org/html/rfc3986#section-2.2."
  { \! "%21"
    \# "%23"
    \$ "%24"
    \& "%26"
    \' "%27"
    \( "%28"
    \) "%29"
    \* "%2A"
    \+ "%2B"
    \, "%2C"
    \/ "%2F"
    \: "%3A"
    \; "%3B"
    \= "%3D"
    \? "%3F"
    \@ "%40"
    \[ "%5B"
    \] "%5D"})

(def ^:private ^:const url-encode-chars
  "This includes some frequently "
  (merge url-encode-chars-strict
         {\tab        "%09"
          \newline    "%0A"
          \return     "%0D"
          \space      "%20"
          \%          "%25"}))

; " < > # % { } | \ ^ ~ [ ] `

(def ^:private ^:const unsafe-chars
  "From rfc1738, Chapter 2.2. URL Character Encoding Issues:

   Characters can be unsafe for a number of reasons. The space character is
   unsafe because significant spaces may disappear and insignificant spaces may
   be introduced when URLs are transcribed or typeset or subjected to the
   treatment of word-processing programs. The characters \"<\" and \">\" are
   unsafe because they are used as the delimiters around URLs in free text; the
   quote mark (\"\"\") is used to delimit URLs in some systems. The character
   \"#\" is unsafe and should always be encoded because it is used in World Wide
   Web and in other systems to delimit a URL from a fragment/anchor identifier
   that might follow it. The character \"%\" is unsafe because it is used for
   encodings of other characters. Other characters are unsafe because gateways
   and other transport agents are known to sometimes modify such characters.
   These characters are \"{\", \"}\", \"|\", \"\\\", \"^\", \"~\", \"[\",
   \"]\", and \"`\".

   All unsafe characters must always be encoded within a URL. For example, the
   character \"#\" must be encoded within URLs even in systems that do not
   normally deal with fragment or anchor identifiers, so that if the URL is
   copied into another system that does use them, it will not be necessary to
   change the URL encoding.

   SOURCE: http://www.ietf.org/rfc/rfc1738.txt
   SOURCE: https://tools.ietf.org/html/rfc1738"
  {\space      "%20"
   \<          "%3C"
   \>          "%3E"
   \"          "%22"
   \%          "%25"
   \{          "%7B"
   \}          "%7D"
   \|          "%7C"
   \\          "%5C"
   \^          "%5E"
   \~          "%7E"
   \`          "%60"})

(def ^:private ^:const url-encode-chars
  (merge url-encode-chars-strict
         {\backspace  "%08"
          \tab        "%09"
          \newline    "%0A"
          \return     "%0D"
          \space      "%20"
          \"          "%22"
          \%          "%25"
          \-          "%2D"
          \.          "%2E"
          \<          "%3C"
          \>          "%3E"
          \\          "%5C"
          \^          "%5E"
          \_          "%5F"
          \`          "%60"
          \{          "%7B"
          \|          "%7C"
          \}          "%7D"
          \~          "%7E"
          \¢          "%A2"
          \£          "%A3"
          \¥          "%A5"
          \¦          "%A6"
          \§          "%A7"
          \«          "%AB"
          \¬          "%AC"
          \¯          "%AD"
          \º          "%B0"
          \±          "%B1"
          \ª          "%B2"
          \´          "%B4"
          \µ          "%B5"
          \»          "%BB"
          \¼          "%BC"
          \½          "%BD"
          \¿          "%BF"
          \À          "%C0"
          \Á          "%C1"
          \Â          "%C2"
          \Ã          "%C3"
          \Ä          "%C4"
          \Å          "%C5"
          \Æ          "%C6"
          \Ç          "%C7"
          \È          "%C8"
          \É          "%C9"
          \Ê          "%CA"
          \Ë          "%CB"
          \Ì          "%CC"
          \Í          "%CD"
          \Î          "%CE"
          \Ï          "%CF"
          \Ð          "%D0"
          \Ñ          "%D1"
          \Ò          "%D2"
          \Ó          "%D3"
          \Ô          "%D4"
          \Õ          "%D5"
          \Ö          "%D6"
          \Ø          "%D8"
          \Ù          "%D9"
          \Ú          "%DA"
          \Û          "%DB"
          \Ü          "%DC"
          \Ý          "%DD"
          \Þ          "%DE"
          \ß          "%DF"
          \à          "%E0"
          \á          "%E1"
          \â          "%E2"
          \ã          "%E3"
          \ä          "%E4"
          \å          "%E5"
          \æ          "%E6"
          \ç          "%E7"
          \è          "%E8"
          \é          "%E9"
          \ê          "%EA"
          \ë          "%EB"
          \ì          "%EC"
          \í          "%ED"
          \î          "%EE"
          \ï          "%EF"
          \ð          "%F0"
          \ñ          "%F1"
          \ò          "%F2"
          \ó          "%F3"
          \ô          "%F4"
          \õ          "%F5"
          \ö          "%F6"
          \÷          "%F7"
          \ø          "%F8"
          \ù          "%F9"
          \ú          "%FA"
          \û          "%FB"
          \ü          "%FC"
          \ý          "%FD"
          \þ          "%FE"
          \ÿ          "%FF"}))

(def ^:private ^:const reserved-chars
  (-> url-encode-chars-strict
      keys
      set))

(defn- encode
  [c]
  (if (reserved-chars c)
    (str "%" (-> c int (Integer/toString 16) upper-case))
    c))

(defn url-encode-2
  "Returns the s string URL encoded. It accepts a 'strict?' flag (false by
  default) to encode only URL reserved characters as per
  https://tools.ietf.org/html/rfc3986#section-2.2.

  (url-encode \"foo\") => \"foo\"
  (url-encode \":foo bar+baz_\") => \"%3Afoo%20bar%2Bbaz%5F\"
  (url-encode \":foo bar+baz_\" :strict) => \"%3Afoo bar%2Bbaz_\""
  ^String [^String s]
  {:pre   [(string? s)]
   :post  [(string? %)]
   }
  (->> s
       (map encode)
       join))

(defn url-encode
  "Returns the s string URL encoded. It accepts a 'strict?' flag (false by
  default) to encode only URL reserved characters as per
  https://tools.ietf.org/html/rfc3986#section-2.2.

  (url-encode \"foo\") => \"foo\"
  (url-encode \":foo bar+baz_\") => \"%3Afoo%20bar%2Bbaz%5F\"
  (url-encode \":foo bar+baz_\" :strict) => \"%3Afoo bar%2Bbaz_\""
  (^String
   [^String s strict?]
   {:pre   [(string? s)]
    :post  [(string? %)]}
   (if-not strict?
     (url-encode s)
     (escape s url-encode-chars-strict)))
  (^String
   [^String s]
   {:pre   [(string? s)]
    :post  [(string? %)]}
   (escape s url-encode-chars)))

(defn- invert-with-default
  "Returns the inverted map m prepared to be used as the replacement function in
  clojure.string/replace, i.e. chars are stringified and it returns the same
  input if it's not found in the map (instead of nil)."
  [m]
  ; NOTE: Chars need to be stringified in order to work with the replace
  ;       function. Otherwise we could direcly use clojure.set/map-invert.
  (let [inverted-char-map (zipmap (vals m) (map str (keys m)))]
    (fn [match]
      (inverted-char-map match match))))

(def ^:private url-decode-strings-strict
  (invert-with-default url-encode-chars-strict))

(def ^:private url-decode-strings
  (invert-with-default url-encode-chars))

(def ^:private re-url-code
  #"%[0-9A-Fa-f]{2}")

(defn url-decode
  "Returns the s string URL decoded. It accepts a 'strict?' flag (false by
  default) to encode only URL reserved characters as per
  https://tools.ietf.org/html/rfc3986#section-2.2.

  (url-decode \"foo\") => \"foo\"
  (url-decode \"%3Afoo%20bar%2Bbaz%5F\") => \":foo bar+baz_\"
  (url-decode \"%3Afoo%20bar%2Bbaz%5F\" :strict) => \":foo%20bar+baz%5F\""
  (^String
   [^String s strict?]
   {:pre   [(string? s)]
    :post  [(string? %)]}
   (if-not strict?
     (url-decode s)
     (replace s re-url-code url-decode-strings-strict)))
  (^String
   [^String s]
   {:pre   [(string? s)]
    :post  [(string? %)]}
   (replace s re-url-code url-decode-strings)))


; -----------------------

(defn- percent-substitution-map
  [char-set]
  (into {} (map #(->> % int Integer/toHexString upper-case (str "%") (vector %))
                char-set)))

(def ^:private ^:const url-reserved-chars
  "From rfc3986, Section 2.2. Reserved Characters:

   URIs include components and subcomponents that are delimited by characters in
   the \"reserved\" set. These characters are called \"reserved\" because they may
   (or may not) be defined as delimiters by the generic syntax, by each scheme-
   specific syntax, or by the implementation-specific syntax of a URI's
   dereferencing algorithm. If data for a URI component would conflict with a
   reserved character's purpose as a delimiter, then the conflicting data must
   be percent-encoded before the URI is formed.

   SOURCE: https://tools.ietf.org/html/rfc3986#section-2.2"
  #{ \! \# \$ \& \'\(\) \* \+ \, \/ \: \; \= \? \@ \[\]})

(def ^:private ^:const url-unsafe-chars
  "From rfc1738, Section 2.2. URL Character Encoding Issues:

   Characters can be unsafe for a number of reasons. The space character is
   unsafe because significant spaces may disappear and insignificant spaces may
   be introduced when URLs are transcribed or typeset or subjected to the
   treatment of word-processing programs. The characters \"<\" and \">\" are
   unsafe because they are used as the delimiters around URLs in free text; the
   quote mark (\"\"\") is used to delimit URLs in some systems. The character
   \"#\" is unsafe and should always be encoded because it is used in World Wide
   Web and in other systems to delimit a URL from a fragment/anchor identifier
   that might follow it. The character \"%\" is unsafe because it is used for
   encodings of other characters. Other characters are unsafe because gateways
   and other transport agents are known to sometimes modify such characters.
   These characters are \"{\", \"}\", \"|\", \"\\\", \"^\", \"~\", \"[\",
   \"]\", and \"`\".

   All unsafe characters must always be encoded within a URL. For example, the
   character \"#\" must be encoded within URLs even in systems that do not
   normally deal with fragment or anchor identifiers, so that if the URL is
   copied into another system that does use them, it will not be necessary to
   change the URL encoding.

   SOURCE: https://tools.ietf.org/html/rfc1738"
  #{\space \< \> \" \# \% \{ \} \| \\ \^ \~ \[ \] \`})

(def ^:private ^:const url-unreserved-chars
  "From rfc1738, Section 2.3: Unreserved Characters

   Characters that are allowed in a URI but do not have a reserved purpose are
   called unreserved. These include uppercase and lowercase letters, decimal
   digits, hyphen, period, underscore, and tilde.

      unreserved  = ALPHA / DIGIT / \"-\" / \".\" / \"_\" / \"~\"

   URIs that differ in the replacement of an unreserved character with its
   corresponding percent-encoded US-ASCII octet are equivalent: they identify
   the same resource. However, URI comparison implementations do not always
   perform normalization prior to comparison (see Section 6). For consistency,
   percent-encoded octets in the ranges of ALPHA (%41-%5A and %61-%7A), DIGIT
   (%30-%39), hyphen (%2D), period (%2E), underscore (%5F), or tilde (%7E)
   should not be created by URI producers and, when found in a URI, should be
   decoded to their corresponding unreserved characters by URI normalizers.

   SOURCE: https://tools.ietf.org/html/rfc3986#section-2.3"
  (-> #{\- \. \_ \~ }
      (into (map char (range (int \a) (int \z))))
      (into (map char (range (int \A) (int \Z))))
      (into (map char (range (int \0) (int \9))))))

(def ^:private ^:const ascii-control-chars
  "From rfc1738, Section 2.2. URL Character Encoding Issues:

   SOURCE: https://tools.ietf.org/html/rfc1738"
  (->> (conj (range 0 31) 127)
       (map char)
       set))

(def ^:private ^:const non-ascii-chars
  "From rfc1738, Section 2.2. URL Character Encoding Issues:

   SOURCE: https://tools.ietf.org/html/rfc1738"
  (->> (range 128 255)
       (map char)
       set))

; SOURCE: http://stackoverflow.com/a/13041851
(defn percent-encode-char
  [c]
  (->> (.getBytes c "UTF-8")
       (map (partial format "%%%02x"))
       (apply str)
       upper-case))

(def ^:private ^:const url-encoding-map
  (percent-substitution-map
    (clojure.set/union url-reserved-chars
                       url-unsafe-chars
                       ascii-control-chars
                       non-ascii-chars)))

(defn url-encode-3
  "Returns the s string URL encoded. It accepts a 'strict?' flag (false by
  default) to encode only URL reserved characters as per
  https://tools.ietf.org/html/rfc3986#section-2.2.

  (url-encode \"foo\") => \"foo\"
  (url-encode \":foo bar+baz_\") => \"%3Afoo%20bar%2Bbaz%5F\"
  (url-encode \":foo bar+baz_\" :strict) => \"%3Afoo bar%2Bbaz_\""
  ^String [^String s]
   {:pre   [(string? s)]
    :post  [(string? %)]}
   (escape s url-encoding-map))


; (println (time (dotimes [_ 1000000] (url-encode ":foo bar+baz_" :strict))))
; ; "Elapsed time: 1636.550868 msecs"
; (println (time (dotimes [_ 1000000] (url-encode-2 ":foo bar+baz_"))))
; ; "Elapsed time: 5345.586232 msecs"
; (println (time (dotimes [_ 1000000] (url-encode-3 ":foo bar+baz_"))))
; ; "Elapsed time: 1142.184097 msecs"
; (println (time (dotimes [_ 1000000] (java.net.URLEncoder/encode ":foo bar+baz_" "UTF-8")))))
; ; "Elapsed time: 606.400508 msecs"
