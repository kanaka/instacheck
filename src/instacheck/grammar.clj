(ns instacheck.grammar
  (:require [instacheck.util :as util]
            [instaparse.core :as instaparse]))

(defn parser->grammar
  "Takes a instaparse parser and returns an instacheck grammar: the
  :grammar value from the parser with a metadata map containing the
  start rule (:start)."
  [parser]
  (with-meta
    (util/remove-key (:grammar parser) :red)
    {:start (:start-production parser)}))

(defn load-grammar
  "Takes an EBNF grammar test string and returns an instacheck
  grammar (via parser->grammar)."
  [ebnf]
  (parser->grammar (instaparse/parser ebnf)))

;;;;;;;;;;;;;;;;;;;

(defn- grammar-seq*
  [path node]
  (let [tag (:tag node)]
    (cond
      (:parsers node) (apply merge (map-indexed
                                     #(grammar-seq* (conj path tag %1) %2)
                                     (:parsers node)))
      (:parser node)  (grammar-seq* (conj path tag) (:parser node))
      :else           {path node})))

(defn grammar-seq
  "Return a map of all grammar paths to grammar nodes."
  [grammar]
  (apply merge (map (fn [[k v]] (grammar-seq* [k] v)) grammar)))

(defn parse-grammar-comments
  "Takes a grammar, reads edn maps from comments in the grammar and
  returns a map of grammar paths to parsed comment data. Multiple
  comments with edn maps at the same path will be merged into a single
  map. If the optional tx function is provided it will be applied to
  the merge maps values.

  This requires a patched version of instaparse to retain the comments
  in the grammar on :comments keys."
  [grammar & [tx]]
  (let [tx (or tx identity)
        path-nodes (grammar-seq grammar)
        path-vals (map (fn [[k v]]
                         (let [c (filter #(re-seq #"^ *\{" %)
                                         (-> v :comments))]
                           (when (seq c)
                             [k (tx (into {} (map read-string c)))])))
                       path-nodes)]
    (into {} path-vals)))
