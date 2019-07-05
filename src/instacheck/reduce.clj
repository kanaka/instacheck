(ns instacheck.reduce
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [instacheck.util :as util]
            [instacheck.grammar :as grammar]
            [instacheck.codegen :as codegen]))

;; ---

(defn- parent-search
  "Internal: Takes a grammar, context ctx (:wtrek, :remove, :removed)
  and node-path and returns an updated context by searching for the
  nearest weighted parent node or nearest parent root :nt of this
  grammar rule tree (i.e. the root NT of this rule/production):
    - weighted parent node: the parent node's child edge leading back
      to the node-path has its weight in (:wtrek ctx) zero'd out. If
      all sibling weights are zero then the parent node is added to
      (:remove ctx).
    - root NT: the root NT keyword is added to (:remove ctx) which is
      then used by propagate-removes to find all grammar leaf nodes
      ending with NT keyword from which the parent-search will be
      continued."
  [grammar ctx node-path]
  (cond
    (= 1 (count node-path)) ;; node-path is an nt
    ;; Expand nt into paths to that nt
    ;; TODO: memoize call to paths-to-nt
    (let [paths-to-nt (grammar/paths-to-nt grammar (first node-path))]
      (-> ctx
          (update-in [:remove] set/union (set/difference (set paths-to-nt)
                                                         (:removed ctx)))
          (update-in [:removed] set/union (set paths-to-nt) #{node-path})))

    (grammar/CHILD-EDGE (last node-path)) ;; node-path is a child edge to parent
    (recur grammar
           (if (grammar/WEIGHTED (last (pop node-path)))
             (update-in ctx [:wtrek] assoc node-path 0)
             ctx)
           (pop node-path))

    (grammar/WEIGHTED (last node-path)) ;; parent node in rule tree
    (if (grammar/removed-node? grammar (:wtrek ctx) node-path)
      (-> ctx
          (update-in [:remove] set/union #{(pop node-path)})
          (update-in [:removed] set/union #{node-path}))
      ctx)

    :else
    (recur grammar ctx (pop node-path))))

;; Weight reducer functions. Must eventually return 0.
(defn reducer-zero
  "Ignores params. Always returns 0"
  [start-weight parsed-weight]
  0)

(defn reducer-half
  "Ignores parsed-weight. Returns start-weight divided in two and
  rounded down."
  [start-weight parsed-weight]
  (int (/ start-weight 2)))

(defn reducer-ladder
  "Ignores parsed-weight. Returns the next weight in seq-ladder that
  is lower than start-weight. Designed to be used as a partial like this:
    (partial reducer-ladder [30 10 3 1])
  The values in the ladder will be sorted in descending order and an
  implicit zero is added to the end."
  [seq-ladder start-weight parsed-weight]
  (let [norm-ladder (-> seq-ladder set (conj 0) sort reverse)]
    (or (some #(if (< % start-weight) % nil) norm-ladder) 0)))

(defn reduce-weights
  "Takes a grammar and wtrek and returns a context map with a :wtrek
  map and a :removed set that describe the removed edges and nodes
  respectively. Any zero weights in the :wtrek map represent a node
  edge that has been removed. Any nodes in the :removed set represent
  root NTs nodes that have been removed.

  A weighted node (:alt, :ord, :opt, or :star) with all child edges is
  a node that has been removed. For each removed node, parent-search
  is called to propagate the removal upwards in the tree to nearest
  weighted parent edge or root NT. If a weighted parent edge is found
  then that edge weight is set to zero. If all siblings of the edge
  also have a zero weight then the parent node is add to the pending
  remove set. If no weighted parent is found (if there are no other
  weighted nodes between the current node and the root) and the search
  reaches a root NT node then that root NT is added to the pending
  remove set.

  The propagation of node removals continues until there are no more
  pending node to remove. The call to parent-search may add more nodes
  to be removed but already removed nodes will not be added again so
  the process will eventually terminate."
  [grammar wtrek]
  (let [removed? (partial grammar/removed-node? grammar wtrek)
        del-nodes (reduce #(if (removed? %2) (conj %1 %2) %1)
                          #{}
                          (set (map (comp pop key) wtrek)))
        ctx (loop [ctx {:wtrek   wtrek
                        :remove  del-nodes
                        :removed #{}}]
              (let [new-ctx (reduce #(parent-search grammar %1 %2)
                                    (assoc ctx :remove #{})
                                    (:remove ctx))]
                (if (not (empty? (:remove new-ctx)))
                  (recur new-ctx)
                  new-ctx)))]
    {:wtrek   (:wtrek ctx)
     :removed (set (filter #(not (grammar/CHILD-EDGE (last %)))
                           (:removed ctx)))}))

(defn reduce-wtrek-with-weights
  "Takes a wtrek, a weights-to-reduce map and a reducer-fn. For each
  path in weights-to-reduce, the reducer-fn is called with the weights
  for that path from wtrek and weights-to-reduce respectively. Based
  on those two values the reducer-fn should return a new value to be
  updated in the wtrek.

  Typically the output from this will then be used with the
  reduce-weights function to propogate any nodes removed by the
  reduction process:
    (reduce-weights grammar (reduce-wtrek-with-weights
                              wtrek weights-to-reduce reducer-fn))"
  [wtrek weights-to-reduce reducer-fn]
  (let [reduced-weights (reduce (fn [a [p rw]]
                                  (let [sw (get wtrek p)]
                                    (assoc a p (reducer-fn sw rw))))
                                {}
                                weights-to-reduce)]
    (merge wtrek reduced-weights)))

;; ---

(defn prune-node*
  "Internal: Used by prune-node* to prune rule bodies/productions
  based on :wtrek"
  [node wtrek cur-path]
  (let [epsilon? #(= :epsilon (:tag %))
        tag (:tag node)]
    (cond
      (and (grammar/CHILD-EDGE (last cur-path))
           (grammar/WEIGHTED (last (pop cur-path)))
           (contains? #{0 nil} (get wtrek cur-path)))
      {:tag :epsilon}

      (:parsers node)  ;; :alt, :cat
      (let [ps (filter
                 #(not (epsilon? %))
                 (map-indexed
                   (fn [idx n]
                     (prune-node* n wtrek (conj cur-path tag idx)))
                   (:parsers node)))]
        (cond
          (= 0 (count ps)) {:tag :epsilon}
          (= 1 (count ps)) (first ps)
          :else            (assoc node :parsers ps)))

      (:parser2 node)  ;; :ord
      (let [p1 (prune-node* (:parser1 node) wtrek (conj cur-path tag 0))
            p2 (prune-node* (:parser1 node) wtrek (conj cur-path tag 1))]
        (cond (and (epsilon? p1)
                   (epsilon? p2)) {:tag :epsilon}
              (epsilon? p1)       p2
              (epsilon? p2)       p1
              :else               (merge node {:parser1 p1 :parser2 p2})))

      (:parser node)  ;; :opt, :start, :plus
      (let [n (prune-node* (:parser node) wtrek (conj cur-path tag 0))]
        (if (epsilon? n)
          n
          (assoc node :parser n)))

      :else  ;; :nt, :string, :regexp, :epsilon
      node)))

(defn prune-grammar
  "Takes a grammar and returns a pruned grammar based on keys
  specified in the options map. Three different prune operations are
  performed:
    - Removes rules listed in :removed
    - Prune rule bodies/productions based on :wtrek
    - If :start is specified in the options or is on the meta of the
      grammar, then rules are removed that cannot be reached (directly
      or indirectly) from the start rule/production.."
  [grammar {:keys [wtrek start removed] :as ctx}]
  (let [wtrek (or wtrek (grammar/wtrek grammar 100))
        start (or start (:start (meta grammar)))
        ;; Remove rules listed in removed
        g1 (select-keys
             grammar
             (set/difference (set (keys grammar)) (set removed)))
        ;; Prune rule bodies using wtrek
        g2 (reduce
             (fn [g [r n]] (assoc g r (prune-node* n wtrek [r])))
             g1
             g1)
        ;; Remove rules that are never reached from start rule
        used (if start
               (let [deps (util/tree-deps g2)]
                 (loop [used #{}
                        pend #{start}]
                   (if (seq pend)
                     (let [new-used (set/union used pend)
                           pend-deps (apply set/union (vals (select-keys deps pend)))
                           new-pend (set/difference pend-deps new-used)]
                       (recur new-used new-pend))
                     used)))
               (set (keys g2)))
        g3 (select-keys g2 used)]
    g3))

