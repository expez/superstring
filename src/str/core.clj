(ns str.core
  (:require clojure.string)
  (:refer-clojure :exclude [reverse replace contains?]))

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
  [^String s ^long index ^long length]
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
  ([^String s ^long index]
   (slice s index 1))
  ([^String s ^long index ^long length]
   (cond
     (neg? length) nil
     (neg? (+ (.length s) index)) nil ; slice relative to end falls outside s
     (neg? index) (slice-relative-to-end s index length)
     (>= index (.length s)) nil
     (> (- length index) (.length (.substring s index))) (.substring s index)
     :else (let [end (+ index length)]
             (.substring s index end)))))

(defn ^String ends-with?
  "Return s if s ends with suffix."
  ([^String s ^String suffix]
   (when (.endsWith s suffix)
     s))
  ([^String s ^String suffix ignore-case]
   (if-not ignore-case
     (ends-with? s suffix)
     (let [end (.substring s (max 0 (- (.length s) (.length suffix))))]
       (when (.equalsIgnoreCase end suffix)
         s)))))

(defn ^String starts-with?
  "Return s if s starts with with prefix.

  If a third argument is provided the string comparison is insensitive to case."
  ([^String s ^String prefix]
   (when (.startsWith s prefix)
     s))
  ([^String s ^String prefix ignore-case]
   (if-not ignore-case
     (starts-with? s prefix)
     (let [beg (.substring s 0 (.length prefix))]
       (when (.equalsIgnoreCase beg prefix)
         s)))))

(defn ^String chop
  "Return a new string with the last character removed.

  If the string ends with \\r\\n, both characters are removed.

  Applying chop to an empty string is a no-op."
  [^String s]
  (if (.endsWith s "\r\n")
    (.substring s 0 (- (.length s) 2))
    (.substring s 0 (max 0 (dec (.length s))))))

(defn ^String chomp
  "Return a new string with the given record separator removed from
  the end (if present).

  If seperator is not provided chomp will remove \\n, \\r or \\r\\n from
  the end of s."
  ([^String s]
   (cond
     (.endsWith s "\r\n") (.substring s 0 (- (.length s) 2))
     (.endsWith s "\r") (.substring s 0 (dec (.length s)))
     (.endsWith s "\n") (.substring s 0 (dec (.length s)))
     :else s))
  ([^String s ^String separator]
   (if (.endsWith s separator)
     (.substring s 0 (- (.length s) (.length separator)))
     s)))

(defn ^String capitalize
  "Return a new string where the first character is in upper case and
  all others in lower case."
  [^String s]
  (case (.length s)
    0 ""
    1 (upper-case s)
    (str (upper-case (.substring s 0 1)) (lower-case (.substring s 1)))) )

(defn ^String swap-case
  "Change lower case characters to upper case and vice versa."
  [^String s]
  (let [invert-case (fn [c]
                      (cond
                        (Character/isLowerCase c) (Character/toUpperCase c)
                        (Character/isUpperCase c) (Character/toLowerCase c)
                        :else c))]
    (->> s (map invert-case) (apply str))))

(defn- gen-padding
  "Generate the necessary padding to fill s upto width."
  [^String s ^String padding ^long width]
  (let [missing (- width (.length s))
        full-lengths (Math/floor (/ missing (.length padding)))
        remaining (if (zero? full-lengths) (- width (.length s))
                      (rem missing (* full-lengths (.length padding))))]
    (.concat (apply str (repeat full-lengths padding))
             (.substring padding 0 remaining))))


(defn ^String pad-right
  "Pad the end of s with padding, or spaces, until the length of s matches
  width."
  ([^String s ^long width]
   (pad-right s width " "))
  ([^String s ^long width ^String padding]
   {:pre [(not-empty padding)
          (not (nil? s))]
    :post [(= (.length %) width)]}
   (if (<= width (.length s))
     s
     (.concat s (gen-padding s padding width)))))

(defn ^String pad-left
  "Pad the beginning of s with padding, or spaces, until the length of
  s matches width."
  ([^String s ^long width]
   (pad-left s width " "))
  ([^String s ^long width ^String padding]
   {:pre [(not-empty padding)
          (not (nil? s))]
    :post [(= (.length %) width)]}
   (if (<= width (.length s))
     s
     (.concat (gen-padding s padding width) s))))

(defn ^String center
  "Pad both ends of s with padding, or spaces, until the length of s
  matches width."
  ([^String s ^long width]
   (center s width " "))
  ([^String s ^long width ^String padding]
   {:pre [(not-empty padding)
          (not (nil? s))]
    :post [(= (.length %) width)]}
   (if (<= width (.length s))
     s
     (let [missing (- width (.length s))
           full-lengths (Math/ceil (/ missing (.length padding)))
           p (gen-padding s padding width)
           lengths-before (Math/floor (/ full-lengths 2))]
       (str (.substring p 0 (* (.length padding) lengths-before))
            s
            (.substring p (* (.length padding) lengths-before)))))))

(defn ^String chop-suffix
  "If s ends with suffix return a new string without the suffix.

  Otherwise return s."
  ([^String s ^String suffix]
   (chop-suffix s suffix false))
  ([^String s ^String suffix ignore-case]
   {:pre [(not (nil? s))
          (not (nil? suffix))]
    :post [(not (nil? %))]}
   (if (and (>= (.length s) (.length suffix))
            (ends-with? s suffix ignore-case))
     (.substring s 0 (- (.length s) (.length suffix)))
     s)))

(defn ^String chop-prefix
  "If s starts with with prefix return a new string without the prefix.

  Otherwise return s."
  ([^String s ^String prefix]
   (chop-prefix s prefix false))
  ([^String s ^String prefix ignore-case]
   {:pre [(not (nil? s))
          (not (nil? prefix))]
    :post [(not (nil? %))]}
   (if (and (>= (.length s) (.length prefix))
            (starts-with? s prefix ignore-case))
     (.substring s (.length prefix))
     s)))
