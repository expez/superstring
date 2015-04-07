(ns str.core)

(defn ends-with? [s suffix]
  {:pre [(string? s)
         (string? suffix)]}
  (when (.endsWith s suffix)
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
   {:pre [(not (nil? s))
          (not (nil? separator))]
    :post [(not (nil? %))]}
   (if (.endsWith s separator)
     (.substring s 0 (- (.length s) (.length separator)))
     s)))
