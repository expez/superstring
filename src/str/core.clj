(ns str.core
  (:require clojure.string)
  (:refer-clojure :exclude [reverse replace]))

;; Taken from [jackknife "0.1.6"]
(defmacro defalias
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

(defmacro alias-ns
  "Create an alias for all public vars in ns in this ns."
  [namespace]
  `(do ~@(map
          (fn [n] `(defalias ~(.sym n) ~(symbol (str (.ns n)) (str (.sym n)))))
          (vals (ns-publics namespace)))))

(alias-ns clojure.string)

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
   {:pre [(string? s)]
    :post [(string? %)]}
   (cond
     (.endsWith s "\r\n") (.substring s 0 (- (.length s) 2))
     (.endsWith s "\r") (.substring s 0 (dec (.length s)))
     (.endsWith s "\n") (.substring s 0 (dec (.length s)))
     :else s))
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

(defn invert-case
  "Change lower case characters to upper case and vice versa."
  [s]
  (let [invert-case (fn [c]
                      (cond
                        (Character/isLowerCase c) (Character/toUpperCase c)
                        (Character/isUpperCase c) (Character/toLowerCase c)
                        :else c))]
    (->> s (map invert-case) (apply str))))
