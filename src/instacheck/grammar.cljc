(ns instacheck.grammar
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.walk :refer [postwalk]]
            #?(:cljs [cljs.reader :refer [read-string]])

            [instacheck.util :as util]
            [instaparse.core :as instaparse]))

;; Instacheck Grammar Handling

(def LEAF-TAGS #{:nt :string :regexp :epsilon})
(def TERMINAL-TAGS #{:string :regexp :epsilon})
(def WEIGHTED  #{:alt :ord :star :opt})
(defn CHILD-EDGE [e] (or (nil? e) (number? e)))
(def NIL-EDGE  #{:star :opt})
(defn TERMINAL [v] (or (string? v)
                       (instance? java.util.regex.Pattern v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar loading/conversion functions

(defn parser->grammar
  "Takes a instaparse parser and returns an instacheck grammar: the
  :grammar value from the parser with a metadata map containing the
  start rule (:start)."
  [parser]
  (with-meta
    (util/remove-key (:grammar parser) :red)
    {:start (:start-production parser)}))

(defn grammar->parser
  "Takes a grammar and returns a instaparse parser. The start
  production must either be set on the grammar metadata (by
  parser->grammar) or must be explicitly provided."
  [grammar & [start]]
  (let [start (or start (:start (meta grammar)))]
    (assert start "start must be either explicit or in grammar metadata")
    (instaparse/parser grammar :start start)))

(defn load-parser
  "Takes an EBNF grammar test string and returns an instacheck
  parser."
  [ebnf]
  (instaparse/parser ebnf))

(defn load-grammar
  "Takes an EBNF grammar test string and returns an instacheck
  grammar (via parser->grammar)."
  [ebnf]
  (parser->grammar (instaparse/parser ebnf)))

(defn grammar->ebnf
  "Takes a grammar and returns the EBNF string equivalent of that
  grammar."
  [grammar]
  (string/join
    "\n"
    (for [[rule node] grammar]
      (instaparse.print/rule->str rule node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar functions

(defn get-in-grammar
  "Get the a grammar node for the given path in grammar. Nil is
  returned if the path does not exist in the grammar, the tag type
  along the path don't match, or the numeric parser index is
  out-of-bounds. Like get-in for grammars."
  [grammar path]
  (loop [g (get grammar (first path))
	 p (rest path)]
    (let [[t1 t2 & ts] p]
      (cond
        (or (empty? p)
            (and (= 1 (count p)) (= t1 (:tag g))))
	g

	(or (nil? g) (not= (:tag g) t1))
	nil

        ;; :alt, :cat
	(and (number? t2) (:parsers g) (> (count (:parsers g)) t2))
	(recur (nth (:parsers g) t2) ts)

        ;; :ord
	(and (= t2 0) (:parser1 g))
	(recur (:parser1 g) ts)
	(and (= t2 1) (:parser2 g))
	(recur (:parser2 g) ts)

        ;; nil path of :opt, :star
        (and (= t2 nil) (#{:opt :star} (:tag g)))
        {:tag :epsilon}

        ;; 0 path of :opt, :star, :plus
        (and (= t2 0) (:parser g))
	(recur (:parser g) ts)

	:else
	nil))))

(defn- update-in-grammar*
  "Internal: walk a grammar rule tree applying tx at each path/node"
  [targ-path path node f args]
  (let [tag (:tag node)]
    (cond
      (= (vec targ-path) path)
      (apply f node args)

      ;; :alt, :cat
      (:parsers node)
      (assoc node
             :parsers (map-indexed
                        #(update-in-grammar*
                           targ-path (conj path tag %1) %2 f args)
                        (:parsers node)))
      ;; :ord
      (:parser2 node)
      (assoc node
             :parser1 (update-in-grammar*
                        targ-path (conj path tag 0) (:parser1 node) f args)
             :parser2 (update-in-grammar*
                        targ-path (conj path tag 1) (:parser2 node) f args))

      ;; :opt, :star, :plus
      (:parser node)
      (assoc node
             :parser (update-in-grammar*
                       targ-path (conj path tag 0) (:parser node) f args))

      ;; :nt, :string, :regexp, :epsilon
      :else
      node)))

(defn update-in-grammar
  "Update a grammar node at the given path in the grammar by running
  (apply f node args) on the node and returning the update grammar.
  Like update-in for grammars."
  [grammar [rule-kw & rest-path :as path] f & args]
  (assoc grammar
         rule-kw
         (update-in-grammar*
           rest-path [] (get grammar rule-kw) f args)))

(defn assoc-in-grammar
  "Update a grammar node at the given path in grammar to a new value.
  Like assoc-in for grammars."
  [grammar path value]
  (update-in-grammar grammar path (fn [n] value)))

(defn apply-grammar-update
  "Takes a grammar and an map of grammar paths to grammar nodes (an
  update-trek). Replaces each update path in the grammar ith the new
  update node."
  [grammar utrek]
  (reduce (fn [g [p n]] (assoc-in-grammar g p n)) grammar utrek))

(declare trek)

(defn paths-to-leaf
  "Given a grammar and a non-terminal keyword nt, return all paths
  within the grammar that have nt as a leaf node."
  [grammar leaf-val]
  (let [gs (trek grammar)
        base-paths (keys (filter #(= leaf-val (val %)) gs))
        ;; Add :opt and :star nil paths and convert to set
        paths (reduce
                (fn [l p]
                  (if (#{:opt :star} (last (pop p)))
                    (conj l p (conj (pop p) nil))
                    (conj l p)))
                #{}
                base-paths)]
    paths))

(def memoized-paths-to-leaf (memoize paths-to-leaf))

(defn get-descendants*
  [grammar path pred avoid]
  (let [node (get-in-grammar grammar path)
        tag (:tag node)]
    (cond
      (or (avoid path) (nil? node))
      []

      (pred path)
      [path]

      (TERMINAL-TAGS tag)
      []

      (= :nt tag)
      (get-descendants* grammar [(:keyword node)] pred (set/union #{path} avoid))

      (or (= 1 (count path))
          (CHILD-EDGE (last path)))
      (get-descendants* grammar (conj path (:tag node)) pred (set/union #{path} avoid))

      :else
      (let [sibs (cond
                   (#{:star :opt} tag) [[nil {:tag :epsilon}] [0 (:parser node)]]
                   (:parser node)      [[0 (:parser node)]]
                   (:parsers node)     (map-indexed vector (:parsers node))
                   (:parser2 node)     [[0 (:parser1 node)] [1 (:parser2 node)]])
            sib-paths (map (fn [[i n]] (conj path i)) sibs)
            avoid (set/union (set sib-paths) avoid)
            sib-desc (map #(get-descendants* grammar % pred (disj avoid %))
                          sib-paths)]
        (vec (apply concat sib-desc))))))

(defn get-descendants
  "Return a set of paths for which each p in the set is a descendent
  of path for which (pred p) is true."
  [grammar path pred]
  (get-descendants* grammar path #(and (pred %) (not= % path)) #{}))

(defn get-ancestors*
  [grammar path pred avoid]
  (cond
    (avoid path)
    #{}

    (pred path)
    #{path}

    (= 1 (count path)) ;; path is an nt root
    (let [paths (memoized-paths-to-leaf grammar (first path))
          avoid (set/union (set paths) avoid)]
      (apply
        set/union
        (map #(get-ancestors* grammar % pred (disj avoid %))
             paths)))

    :else
    (recur grammar (pop path) pred (set/union #{path} avoid))))

(defn get-ancestors
  "Returns a vector of paths for which each p in the vector is
  a parent edge of path in grammar for which (pred p) is true. For
  example, to find the nearest parent :cat nodes of my-path:
  (get-ancestors g my-path #(= :cat (last %)))"
  [grammar path pred]
  (let [path (if (= 1 (count path)) path (pop path))]
    (get-ancestors* grammar path pred #{})))

(defn children-of-node
  "Given a grammar and path node within that grammar return the paths
  of the children (:alt, :ord, and :cat nodes)."
  [grammar path]
  (seq (get-descendants grammar path #(CHILD-EDGE (last %)))))

(defn get-weighted-ancestors
  [grammar path]
  (get-ancestors grammar path #(WEIGHTED (last %))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; trek functions

(defn- node-to-val
  "Internal: convert a grammar leaf node to a scalar: keyword, string,
  regexp, or epsilon (empty string)."
  [node default]
  (let [tag (:tag node)]
    (cond (= :nt tag)       (:keyword node)
          (= :string tag)   (:string node)
          (= :regexp tag)   (:regexp node)
          (= :epsilon tag)  ""
          :else             default)))

(defn- val-to-node
  "Internal: convert a scalar keyword, empty string (epsilon), string,
  or regexp to a grammar leaf node."
  [v]
  (cond
    (instance? java.util.regex.Pattern v) {:tag :regexp :regexp v}
    (and (string? v) (empty? v))          {:tag :epsilon}
    (string? v)                           {:tag :string :string v}
    (keyword? v)                          {:tag :nt     :keyword v}))

(defn- trek-rule*
  "Internal: walk a grammar rule tree applying tx at each path/node"
  [path node tx]
  (let [tag (:tag node)]
    (merge
      (tx path node)
      (cond
        ;; :alt, :cat
        (:parsers node)
        (apply merge (map-indexed
                       #(trek-rule* (conj path tag %1) %2 tx)
                       (:parsers node)))

        ;; :ord
        (:parser2 node)
        (merge (trek-rule* (conj path tag 0) (:parser1 node) tx)
               (trek-rule* (conj path tag 1) (:parser2 node) tx))

        ;; :opt, :star
        (#{:opt :star} tag)
        (merge (trek-rule* (conj path tag nil) {:tag :epsilon} tx)
               (trek-rule* (conj path tag 0) (:parser node) tx))

        ;; :plus
        (:parser node)
        (trek-rule* (conj path tag 0) (:parser node) tx)

        ;; :nt, :string, :regexp, :epsilon
        :else
        nil))))

(defn trek-grammar
  "Walk a grammar graph applying tx at each path/node"
  [grammar tx]
  (apply merge (map (fn [[k v]] (trek-rule* [k] v tx)) grammar)))

(defn trek
  "Return a trek of the grammar (map of paths to node values)."
  [grammar]
  (trek-grammar grammar (fn [p n] (when (LEAF-TAGS (:tag n))
                                     {p (node-to-val n n)}))))

(defn comment-trek
  "Return a trek with comment values (map of grammar paths to parsed
  comments) by reading edn maps from comments in the grammar. Note
  that this will return different paths than a normal trek because
  a comments can occur on any :alt/:ord nodes and since comments can
  occur in internal :alt/:ord nodes a comment trek will have these
  internal nodes (a regular trek will not).

  Multiple comments with edn maps in the same node will be merged into
  a single map. If the optional comment-tx function is provided it
  will be applied to the merge maps values.

  This requires a patched version of instaparse to retain the comments
  in the grammar on :comments keys."
  [grammar & [comment-tx]]
  (let [comment-tx (or comment-tx identity)
        tx-fn (fn [p n]
                (let [c (filter #(re-seq #"^ *\{" %)
                                (-> n :comments))]
                  (when (seq c)
                    {p (comment-tx (into {} (map read-string c)))})))]
    (trek-grammar grammar tx-fn)))

(defn- path->tree*
  "Internal: Takes a rule node tree a rest-p path within that tree
  and a final leaf node and returns an updated version of the tree
  with the nodes added that are listed rest-p along with the final
  leaf value. Note that the returned tree is has :parsers values as
  a map rather than a sequence so this must be normalized before it
  can be used as a grammar."
  [tree rest-p leaf]
  (let [[kind idx & rest-p] rest-p]
    (if (not idx)
      leaf
      (cond
        (#{:cat :alt} kind)
        (-> tree
            (assoc :tag kind)
            (assoc-in [:parsers idx]
                      (path->tree* (get-in tree [:parsers idx]) rest-p leaf)))

        (= :ord kind)
        (let [pkw (keyword (str "parser" (inc idx)))]
          (-> tree
              (assoc :tag kind)
              (assoc-in [pkw]
                        (path->tree* (get-in tree [pkw]) rest-p leaf))))

        (#{:star :opt :plus} kind)
        (-> tree
            (assoc :tag kind)
            (assoc-in [:parser]
                      (path->tree* (get-in tree [:parser]) rest-p leaf)))))))

(defn trek->grammar
  "Take a trek and convert it back into a grammar"
  [trek]
  (let [ntrek (into {} (for [[k v] trek] [k (val-to-node v)]))]
    (into
      {}
      (for [[rule-kw rules] (group-by (comp first key) ntrek)]
        [rule-kw
         ;; Build up the tree one path at a time by reducing using
         ;; path->tree*. Then convert each :parsers val from an
         ;; idx->val map to a sorted list of vals.
         (postwalk
           #(if (:parsers %)
              (assoc % :parsers (vals (sort-by key (:parsers %))))
              %)
           (reduce
             (fn [tree [p leaf]]
               (path->tree* tree (subvec p 1) leaf))
             nil
             rules))]))))

