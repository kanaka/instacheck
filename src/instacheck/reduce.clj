(ns instacheck.reduce
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [instacheck.util :as util]
            [instacheck.grammar :as grammar]
            [instacheck.weights :as weights]
            [instacheck.codegen :as codegen]))

(def memoized-distance-trek
  (memoize weights/distance-trek))

(defn reduce-wtrek
  "Takes a grammar and wtrek and returns a new reduced wtrek with all
  parent weights reduced to their largest child weight (if all child
  weights are smaller).

  If the optional reduced-subset is then only those node will be
  propagated. If reduced-subset is not specified then all
  reducible/weighted nodes will be considered. The former may result
  in a wtrek that is not fully reduced but the latter can take a while
  for large grammars/wtreks.

  Algorithm/Pseudocode:
    - pend <= reduced-subset OR all weighted nodes in the tree
    - while pend:
      - node   <= pop(pend)
      - mcw    <= get max child weight
      - pnodes <= parents(node)
      - foreach pnode of pnodes:
        - if pnode child weight towards node > mcw
          - then:
            - push(pend, pnode)
            - wtrek[pnode] <= mcw

  In other words, if all siblings of a node have a weight that is less
  than parent weight then reduce the parent to the largest sibling
  weight.

  Any zero weights in the :wtrek map represent a node edge that has
  been removed. If all edges of a node are 0 then this is represents
  a node that has been removed. The normal algorithm will propagate
  this correctly. However, if the propagation of 0 weights reaches the
  root/start of the grammar and cannot propagate further then an
  exception is thrown because this represents an invalid weighted
  grammar; grammar productions could reach the removed node from the
  root/start rule (a removed node does not even exist in the sense
  that epsilon does).

  The propagation of node removals continues until there are no more
  pending node to remove. The call to parent-search may add more nodes
  to be removed but already removed nodes will not be added again so
  the process will eventually terminate."
  [grammar wtrek & [reduced-subset]]
  (loop [wtrek wtrek
         pend (set (filter #(grammar/WEIGHTED (last %))
                           (map (comp pop key)
                                (or reduced-subset
                                    (weights/wtrek grammar)))))]
    (if (seq pend)
      (let [[node & pend-left] pend
            kids (grammar/children-of-node grammar node)
            kid-weights (vals (select-keys wtrek kids))
            max-kid-w (apply max kid-weights)
            nparents (grammar/get-parents
                       grammar node #(grammar/WEIGHTED (last (pop %))))
            big-parents (set (for [[p n] (select-keys wtrek nparents)
                                   :when (> (get wtrek p) max-kid-w)]
                               p))
;;            _ (prn :node node :max-kid-w max-kid-w :nparents nparents :big-parents big-parents)
            ;; Removed node must have at least one weighted parent in
            ;; nparents that is not self-recursive (i.e. not lower
            ;; than node in the same rule). If there are none than
            ;; this indicates that node has no parents between itself
            ;; and the root/start of the grammar.
            nonrecur-nparents (filter #(not (= (take (count node) %) node))
                                      nparents)
            _ (when (and (= 0 max-kid-w) (not (seq nonrecur-nparents)))
                (throw (Exception.
                         (str "Node " node " removed, has no parents"))))
            new-pend (set/union pend-left
                                (set (map pop big-parents)))
            new-wtrek (reduce (fn [nw [p w]]
                                (if (contains? big-parents p)
                                  (assoc nw p max-kid-w)
                                  (assoc nw p w)))
                              {}
                              wtrek)]
        (recur new-wtrek new-pend))
      wtrek)))

;; ---------

;; Weight reducer functions. If parsed-weight is zero ignore (return
;; start-weight). Must eventually return 0.
(defn reducer-zero
  "If parsed-weight > 0 then returns 0"
  [start-weight parsed-weight]
  (if (= parsed-weight 0)
    start-weight
    0))

(defn reducer-half
  "If parsed-weight > 0 then returns start-weight divided in two and
  rounded down."
  [start-weight parsed-weight]
  (if (= parsed-weight 0)
    start-weight
    (int (/ start-weight 2))))

(defn reducer-ladder
  "If parsed-weight > 0 then returns the next weight in seq-ladder
  that is lower than start-weight. Designed to be used as a partial
  like this:
      (partial reducer-ladder [30 10 3 1])
  The values in the ladder will be sorted in descending order and an
  implicit zero is added to the end."
  [seq-ladder start-weight parsed-weight]
  (if (= parsed-weight 0)
    start-weight
    (let [norm-ladder (-> seq-ladder set (conj 0) sort reverse)]
      (or (some #(if (< % start-weight) % nil) norm-ladder) 0))))



(defn reduce-wtrek-with-weights
  "Takes a grammar, wtrek, a weights-to-reduce map, a reduce-mode
  keyword, and a reducer-fn. A path from weights-to-reduce is selected
  based on reduce-mode (and grammar). For that path the reducer-fn is
  called with the weight for the path from wtrek and the weight for
  the path from weights-to-reduce. Based on those two values the
  reducer-fn should return a new value to be updated in the wtrek.

  reduce-mode values:
    :leaf - one deepest and highest weight terminal path is reduced.
    :path - similar to leaf but if parent has greater weight, it will
            be reduced to the child level, otherwise child is reduced.

  Typically the output from this will then be used with the
  reduce-wtrek function to propogate any nodes removed by the
  reduction process:
    (reduce-wtrek
      grammar
      (reduce-wtrek-with-weights
        grammar wtrek weights-to-reduce :simple reducer-fn))"
  [grammar wtrek weights-to-reduce reduce-mode reducer-fn]
  (let [big? #(and % (> % 0))
        bigs (filter #(and (big? (get weights-to-reduce %))
                           (big? (get wtrek %)))
                     (keys weights-to-reduce))
;;        _ (prn :bigs)
;;        _ (pprint bigs)
        distances (memoized-distance-trek grammar)
        grouped (group-by #(or (get wtrek %) 0)
                          bigs)
;;        (prn :grouped)
;;        (pprint (sort-by key grouped))
        weighted-paths (for [[w ps] grouped
                             p ps]
                         [p (* w (condp = reduce-mode
                                   ;; choice weighted by weight
                                   :weight 1
                                   ;; choice weighted by weight * distance
                                   :weight-dist (get distances p)))])
        rpath (when (seq grouped)
                (util/weighted-rand-nth weighted-paths))]
;;    (prn :rpath rpath :wtrek-w (get wtrek rpath) :wtr-w (get weights-to-reduce rpath))
    (if rpath
      (assoc wtrek rpath (reducer-fn
                           (get wtrek rpath)
                           (get weights-to-reduce rpath)))
      (do
;;        (println "******************* no rpath *******************")
        wtrek)))

  )

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
  (let [wtrek (or wtrek (weights/wtrek grammar 100))
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

