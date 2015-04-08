(ns str.core
  (:require clojure.string))

(def lower-case clojure.string/lower-case)
(def upper-case clojure.string/upper-case)

(defn ends-with?
  "Return s if s ends with suffix."
  [s suffix]
  {:pre [(string? s)
         (string? suffix)]
   :post [(or (string? %) (nil? %))]}
  (when (.endsWith s suffix)
    s))

(defn starts-with?
  "Return s if s starts with with prefix."
  [s prefix]
  {:pre [(string? s)
         (string? prefix)]
   :post [(or (string? %) (nil? %))]}
  (when (.startsWith s prefix)
    s))

(defn chop
  "Return a new string with the last character removed.

  If the string ends with \\r\\n, both characters are removed.

  Applying chop to an empty string is a no-op."
  [s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (if (.endsWith s "\r\n")
    (.substring s 0 (- (.length s) 2))
    (.substring s 0 (max 0 (dec (.length s))))))

(defn chomp
  "Return a new string with the given record separator removed from
  the end (if present).

  If seperator is not provided chomp will remove \\n, \\r or \\r\\n from
  the end of s."
  ([s]
   (cond
     (.endsWith s "\r\n") (.substring s 0 (- (.length s) 2))
     (.endsWith s "\r") (.substring s 0 (dec (.length s)))
     (.endsWith s "\n") (.substring s 0 (dec (.length s)))))
  ([s separator]
   {:pre [(string? s)
          (string? separator)]
    :post [(string? %)]}
   (if (.endsWith s separator)
     (.substring s 0 (- (.length s) (.length separator)))
     s)))

(defn capitalize
  "Return a new string where the first character is in upper case and
  all others in lower case."
  [s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (case (.length s)
    0 ""
    1 (upper-case s)
    (str (upper-case (.substring s 0 1)) (lower-case (.substring s 1)))) )
