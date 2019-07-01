(ns instacheck.grammar
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.walk :refer [postwalk]]
            [instacheck.util :as util]
            [instaparse.core :as instaparse]))

;; Instacheck Grammar and Weights Handling

(def LEAF-TAGS #{:nt :string :regexp :epsilon})
(def WEIGHTED  #{:alt :ord :star :opt})
(def NIL-EDGE  #{:star :opt})

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

(defn load-grammar
  "Takes an EBNF grammar test string and returns an instacheck
  grammar (via parser->grammar)."
  [ebnf]
  (parser->grammar (instaparse/parser ebnf)))

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

        ;; :opt, :star, :plus
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

(defn children-of-node
  "Given a grammar and path node within that grammar return the paths
  of the children (:alt, :ord, and :cat nodes)."
  [grammar node-path]
  (let [node (get-in-grammar grammar node-path)
        base-path (if (number? (last node-path))
                    (conj node-path (:tag node))
                    node-path)
        sibs (cond
               (:parser node)  [(:parser node)]
               (:parsers node) (:parsers node)
               (:parser2 node) ((juxt :parser1 :parser2) node)
               :else [])
        children-paths (map-indexed (fn [i n]
                                      (conj base-path i))
                                    sibs)]
    (seq children-paths)))

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
        (:parsers node)  (apply merge (map-indexed
                                        #(trek-rule* (conj path tag %1) %2 tx)
                                        (:parsers node)))
        ;; :ord
        (:parser2 node)  (merge (trek-rule* (conj path tag 0) (:parser1 node) tx)
                                (trek-rule* (conj path tag 1) (:parser2 node) tx))

        ;; :opt, :star, :plus
        (:parser node)   (trek-rule* (conj path tag 0) (:parser node) tx)

        ;; :nt, :string, :regexp, :epsilon
        :else            nil))))

(defn- trek-grammar*
  "Internal: walk a grammar graph applying tx at each path/node"
  [grammar tx]
  (apply merge (map (fn [[k v]] (trek-rule* [k] v tx)) grammar)))

(defn trek
  "Return a trek of the grammar (map of paths to node values)."
  [grammar]
  (trek-grammar* grammar (fn [p n] (when (LEAF-TAGS (:tag n))
                                     {p (node-to-val n n)}))))

(defn comment-wtrek
  "Return a wtrek with comment values (map of grammar paths to parsed
  comments) by reading edn maps from comments in the grammar. Note
  that this will return different paths than a normal trek because
  a comments can occur on any :alt/:ord nodes and since comments can
  occur in internal :alt/:ord nodes a comment wtrek will have these
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
    (trek-grammar* grammar tx-fn)))

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

(defn paths-to-nt
  "Given a grammar and a non-terminal keyword nt, return all paths
  within the grammar that have nt as a leaf node."
  [grammar nt]
  (let [gs (trek grammar)
        nt-leafs (filter #(and (keyword? (val %))
                               (= nt (val %))) gs)]
    (seq (map key nt-leafs))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; weight functions

(defn removed-node?
  "Takes path to a grammar node and returns true if all the child
  weights of this node are 0. Only paths ending in :alt, :ord, :opt,
  and :star can have child weights)."
  [grammar weights path]
  (let [children (children-of-node grammar path)
	all-zero (every? #(= 0 (get weights %)) children)]
    (if (and (seq children)
	     all-zero)
      path
      nil)))

(defn filter-trek-weighted
  "Given trek, only keep paths that refer to weighted nodes (:alt,
  :ord, :opt, :star). In a generator context, these are the nodes that
  use (igen/freq)."
  [trek]
  (into {} (filter (fn [[p v]] (WEIGHTED (last (pop p)))) trek)))

(defn- expand-nil-edge-paths
  "Internal: takes a trek structure and a weight-fn and returns a new
  trek with nil paths added for each nil-edge node in a trek paths. The
  weight-fn does the actual assoc of the new nil paths and determines
  what their weight value should be."
  [trek weight-fn]
  (let [expand-1 (fn [p]
                   (into
                     [p]
                     (for [idx (keep-indexed #(when (NIL-EDGE %2) %1) p)]
                       (conj (subvec p 0 (inc idx)) nil))))]
    (reduce
      (fn [tk [p _]] (reduce weight-fn tk (expand-1 p)))
      {}
      trek)))

(defn- wtrek-without-comment-weights*
  "Internal: Return an wtrek/weight trek (map of grammar paths to
  weight values). Weights will have a weight of default-weight if
  specified, otherwise 100. Note that this will return different paths
  than a normal trek because a wtrek contains all weighted nodes
  (:alt, :ord, :opt, :star) from the grammar not just leaf nodes."
  [grammar & [default-weight]]
  (let [dw (or default-weight 100)
        full-trek (trek-grammar* grammar (fn [p n] {p 0}))
        ;; add NIL-EDGE nil edge paths with default-weight weight
        new-trek (expand-nil-edge-paths
                   full-trek
                   #(assoc %1 %2 dw))]
    #_(pprint new-trek)
    (filter-trek-weighted new-trek)))

(defn wtrek
  "Takes a grammar and returns a wtrek/weight trek (map of grammar
  paths to weight values). If the node at a path has a comment with
  a :weight specification then this will be used for the weight
  otherwise the default-weight parameter will be used (with a default
  of 100). Note that this will return different paths than a normal
  trek because a wtrek contains all weighted nodes (:alt, :ord, :opt,
  :star) from the grammar not just leaf nodes."
  [grammar & [default-weight]]
  (let [dw (or default-weight 100)]
    (merge (wtrek-without-comment-weights* grammar dw)
           (comment-wtrek grammar :weight))))

(defn path-log-trek
  "Takes a grammar and parse-result parsed using that grammar and
  returns a path-log trek based on the :path-log in parse-result. Note
  that this will return a different set of paths than a normal trek or
  wtrek because it contains \"weights\" for all nodes of the grammar
  and not just for leaf or weighted nodes."
  [grammar parse-result]
  (let [weights (-> parse-result meta :path-log frequencies)
        full-trek (trek-grammar* grammar (fn [p n] {p 0}))
        ;; add NIL-EDGE nil edge paths with nil weight
        sparse-trek (expand-nil-edge-paths
                      full-trek
                      (fn [tk p]
                        (if (NIL-EDGE (last (pop p)))
                          (assoc tk p (get weights p))
                          (assoc tk p (get weights p 0)))))
        new-trek (reduce
                   (fn [tk [p w]]
                     (if (nil? (last p))
                       (let [p0  (conj (pop p) 0)
                             wp0 (get weights p0 0)
                             pp  (pop (pop p))
                             wpp (get weights pp 0)]
                         (if wp0
                           (assoc tk
                                  p (if (#{:star} (last (pop p)))
                                      ;; TODO: :star nil weight is
                                      ;; fuzzy, to be less fuzzy would
                                      ;; probably require using the
                                      ;; order of :path-log entries
                                      (max (- wpp wp0) (- wp0 wpp))
                                      (- wpp wp0))
                                  p0 wp0)
                           (assoc tk
                                  p wpp
                                  p0 0)))
                       (if w
                         (assoc tk p w)
                         tk)))
                   {}
                   sparse-trek)]
    #_(pprint new-trek)
    new-trek))

(defn path-log-wtrek
  "Takes a grammar and parse-result parsed using that grammar and
  returns a wtrek with weights set based on the :path-log in
  parse-result. Note that this will return different paths than
  a normal trek because a wtrek contains all weighted nodes (:alt,
  :ord, :opt, :star) from the grammar not just leaf nodes."
  [grammar parse-result]
  (filter-trek-weighted (path-log-trek grammar parse-result)))

;; Misc

(defn print-weights [path weights]
  (let [sm (sorted-map-by #(compare (str %1) (str %2)))]
    (pprint (into sm weights))))

(defn save-weights [path weights]
  (io/make-parents path)
  (spit path (with-out-str (print-weights path weights))))

