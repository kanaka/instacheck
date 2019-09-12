(ns instacheck.parse
  (:require [clojure.string :as string]
            [instaparse.core :as instaparse]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better instaparse errors

(defn- elide-line
  [text cursor max-length cont-text]
  (let [span (/ max-length 2)
        start (max 0            (- cursor span))
        end   (min (count text) (+ cursor span))]
    (str
      (when (> cursor span) cont-text)
      (subs text start end)
      (when (> (count text) max-length) cont-text))))

(defn- concise-fail-str
  [failure text]
  (let [err-str (with-out-str (instaparse.failure/pprint-failure failure))
        column (:column failure)
        [pos text-line mark-line & reasons] (string/split err-str #"\n")
        text-line (string/replace text-line #"\t" " ")
        text-line (elide-line text-line column 200 "...")
        mark-line (elide-line mark-line column 200 "   ")

        reasons (string/join "\n" reasons)]
    (string/join "\n" [pos text-line mark-line reasons])))

(defn parse
  "Use parser to parse text. On success returns the parsed AST. On
  error, throws an ex-info object with a friendly error location
  string and a info map containing the :failure (instaparse Failure
  object), :text (the original text), and :location (optional location
  parameter)."
  [parser text & [location]]
  (let [res (parser text)]
    (if (instance? instaparse.gll.Failure res)
      ;;(throw (Exception. (concise-fail-str res text)))
      (throw (ex-info (concise-fail-str res text)
                      {:failure res
                       :text text
                       :location location}))
      res)))

