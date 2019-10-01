(ns instacheck.reduce
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [instacheck.util :as util]
            [instacheck.grammar :as grammar]
            [instacheck.weights :as weights]
            [instacheck.codegen :as codegen]))

(def memoized-distance-trek
  (memoize weights/distance-trek))

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

(defn reducer-div
  "If parsed-weight > 0 then returns the next weight in seq-ladder
  that is lower than start-weight.Designed to be used as a partial
  like this:
      (partial reducer-div 2)"
  [divisor start-weight parsed-weight]
  (if (= parsed-weight 0)
    start-weight
    (int (/ start-weight divisor))))

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


(defn reduce-wtrek
  "Takes a grammar and wtrek and returns a new reduced wtrek with
  weights reduced/propagated according to reduce-mode.

  If the optional reduced-subset node set is then only those nodes
  will be propagated. If reduced-subset is not specified then all
  reducible/weighted nodes will be considered. The former may result
  in a wtrek that is not fully reduced but the latter can take a while
  for large grammars/wtreks.

  The way that weights are reduced/propagated depends on reduce-mode:

    :zero
      If all siblings of a node have a zero weight, reduce parent edge
      weights to zero.

      Algorithm/psuedocode:
      - pend := reduced-subset OR all weighted nodes in the tree
      - while pend:
        - node   := pop(pend)
        - mcw    := max-child-weight(node)
        - if mcw > 0: continue at while
        - foreach pnode of parents(node):
          - push(pend, pnode)
          - wtrek[pnode] := mcw

    :max-child:
      If all siblings of a node have a weight that is less
      than parent edge weight then reduce the parent edge weights to
      the largest sibling weight.

      Algorithm/psuedocode:
      - pend := reduced-subset OR all weighted nodes in the tree
      - while pend:
        - node   := pop(pend)
        - mcw    := max-child-weight(node)
        - foreach pnode of parents(node):
          - if pnode child weight towards node > mcw
            - then:
              - push(pend, pnode)
              - wtrek[pnode] := mcw

    :reducer:
      When all siblings of a node are zero, reduce parent edge weights
      by reducer-fn function and distribute the removed weights to
      valid (no removed descendant) child edges of node.

      Algorithm/psuedocode:
      - pend := reduced-subset OR all weighted nodes in the tree
      - while pend:
        - node   := pop(pend)
        - mcw    := max-child-weight(node)
        - if mcw > 0: continue at while
        - acc    := 0
        - foreach pnode of parents(node):
          - tmp := wtrek[pnode]
          - wtrek[pnode] := reducer-fn(wtrek[pnode])
          - acc  += tmp - wtrek[pnode]
          - if max-child-weight(pnode) == 0:
            - push(pend, pnode)
        - cnodes := children-with-no-removed-descendants(node)
        - foreach cnode of cnodes:
          - wtrek[code] += acc / count(cnodes)

  Any zero weights in the :wtrek map represent a node edge that has
  been removed. If all edges of a node are 0 then this is represents
  a node that has been removed and the removal must be propagated up
  the tree to the next weighted node edge. If the propagation of
  removed nodes (0 weights) reaches the root/start of the grammar and
  cannot propagate further then an exception is thrown because this
  represents an invalid weighted grammar: grammar productions could
  reach the removed node from the root/start rule (a removed node does
  not exist in the sense that epsilon does).

  The propagation of node removals continues until there are no more
  pending node to remove. A node may have more than one parent which
  means the number of nodes being considered during propagation may
  increase temporarily but already removed nodes will not be added
  again so the process will eventually terminate."
  [grammar start-wtrek & [{:keys [reduced-subset reduce-mode reducer-fn]
                           :or {reduce-mode :zero
                                reducer-fn reducer-zero}
                           :as opts}]]
  (assert (#{:zero :max-child :reducer} reduce-mode)
          (str "Invalid :reduce-mode " reduce-mode))
  (when (= :reducer reduce-mode)
    (assert reducer-fn ":reducer reduce-mode requires reducer-fn"))
  (let [grammar-start [(:start (meta grammar))]
        start-pend (set (filter #(grammar/WEIGHTED (last %))
                                (map pop
                                     (or reduced-subset
                                         (keys (weights/wtrek grammar))))))]
    (loop [wtrek start-wtrek
           pend start-pend]
      (if (not (seq pend))
        wtrek
        (let [[node & pend-left] pend
              kids (grammar/children-of-node grammar node)
              kid-weights (vals (select-keys wtrek kids))
              ;;            _ (prn :node node :kids kids :kid-weights kid-weights)
              max-kid-w (apply max kid-weights)
              ;; nparents is weighted child edges of parents leading
              ;; to node. all-nparents is the same but includes the
              ;; grammar root path if there is no weighted path
              ;; between node and the root.
              all-nparents (grammar/get-ancestors
                             grammar node #(or (= grammar-start %)
                                               (and (grammar/WEIGHTED (last (pop %)))
                                                    (contains? wtrek %))))
              nparents (disj all-nparents grammar-start)
              ;; big-nparents is reduction candidates. max-kid-w being
              ;; greater than 0 only applies in the :max-child case.
              big-nparents (set (filter #(> (get wtrek %) max-kid-w)
                                       nparents))]
          ;;(prn :node node :max-kid-w max-kid-w :nparents nparents :big-parent big-nparents)
          ;; If node is removed and there are no weighted nodes
          ;; between it and the grammar start/root then it's an
          ;; invalid state.
          (when (and (= 0 max-kid-w)
                     (contains? all-nparents grammar-start))
            (throw (ex-info
                     (str "Node " node " removed, has root as parent")
                     {:type :reduce-wtrek
                      :cause :no-parents
                      :grammar grammar
                      :start-wtrek start-wtrek
                      :opts opts
                      :start-pend start-pend
                      :node node
                      :wtrek wtrek})))

          (cond
            ;; :zero and :reducer reduce-mode only apply when all
            ;; children are zero. :max-child reduce-mode applies
            ;; whenever the largest child is less than the parent.
            (and (#{:zero :reducer} reduce-mode)
                 (not= 0 max-kid-w))
            (recur wtrek pend-left)

            (#{:zero :max-child} reduce-mode)
            (let [new-pend (set/union pend-left
                                      (set (map pop big-nparents)))
                  new-wtrek (reduce (fn [tk p] (assoc tk p max-kid-w))
                                    wtrek
                                    big-nparents)]
              (recur new-wtrek new-pend))

            :reducer
            ;; All children of node are zero at this point.
            (let [new-wtrek1 (reduce (fn [tk p]
                                       (let [w (get tk p)]
                                         (assoc tk p (reducer-fn w w))))
                                     wtrek
                                     big-nparents)
                  zerod-parents (set (filter #(and (= 0 (get new-wtrek1 %))
                                                   (not= (get wtrek %)
                                                         (get new-wtrek1 %)))
                                            big-nparents))
                  ;; we need to recur to check zero'd parents
                  new-pend (set/union pend-left
                                      (set (map pop zerod-parents)))
                  ;; accumulate the total reduction (might be multiple
                  ;; parents reduced)
                  acc-weights (reduce
                                #(+ %1 (- (get wtrek %2) (get new-wtrek1 %2)))
                                0
                                big-nparents)
                  ;; only consider kids with no removed descendants
                  removed? (partial weights/removed-node? grammar new-wtrek1)
                  valid-kids (filter (fn [k]
                                       (empty? (grammar/get-descendants
                                                 grammar k removed?)))
                                     kids)
                  ;;_ (prn :zerod-parents zerod-parents :acc-weights acc-weights)
                  ;; Distribute weight evenly to the valid children
                  new-wtrek2 (reduce
                               (fn [tk kid]
                                 (assoc tk kid
                                        (int (Math/ceil
                                               (/ acc-weights
                                                  (count valid-kids))))))
                               new-wtrek1
                               valid-kids)]
              ;;            (prn :reducer :node node :big-nparents big-nparents :zerod-parents zerod-parents)
              (recur new-wtrek2 new-pend))))))))

;; ---------

(defn reduce-wtrek-with-weights
  "Takes a grammar, wtrek, a weights-to-reduce map, a reduce-mode
  keyword, and a reducer-fn. A path from weights-to-reduce is selected
  based on pick-mode. For that path the reducer-fn is called with the
  weight for the path from wtrek and the weight for the path from
  weights-to-reduce. Based on those two values the reducer-fn should
  return a new value to be updated in the wtrek.

  pick-mode values:
    :weight      - randomly pick a node weighted by node weights.
    :dist        - randomly pick a node weighted by node distances
                   from the start node
    :weight-dist - randomly pick a node weighted by node weights
                   multiplied by node distances from the start node.

  The resulting wtrek will then be passed to the reduce-wtrek function
  to propogate the weight reduction according reduce-mode."
  [grammar wtrek weights-to-reduce
   & [{:keys [reduce-mode reducer-fn pick-mode pick-pred rnd-obj]
       :or {reduce-mode :zero
            reducer-fn reducer-half
            pick-mode :weight-dist
            pick-pred identity}
       :as opts}]]
  (assert (#{:weight :dist :weight-dist} pick-mode)
          (str "Invalid :pick-mode " pick-mode))
  (let [big? #(and % (> % 0))
        bigs (filter #(and (big? (get weights-to-reduce %))
                           (big? (get wtrek %)))
                     (keys weights-to-reduce))
;;        _ (prn :bigs)
;;        _ (pprint bigs)
        distances (memoized-distance-trek grammar)
        grouped (group-by #(or (get wtrek %) 0)
                          bigs)
;;        _ (prn :distances distances)
;;        _ (prn :grouped grouped)
;;        _ (pprint (sort-by key grouped))
        weighted-paths (for [[w ps] grouped
                             p ps
                             :when (pick-pred p)]
                         [p (condp = pick-mode
                              :weight w
                              :dist (get distances p)
                              :weight-dist (* w (get distances p)))])
        rpath (when (seq weighted-paths)
                (util/weighted-rand-nth weighted-paths rnd-obj))]
;;    (prn :rpath rpath :wtrek-w (get wtrek rpath) :wtr-w (get weights-to-reduce rpath))
    (if rpath
      (let [new-wtrek (assoc wtrek rpath (reducer-fn
                                           (get wtrek rpath)
                                           (get weights-to-reduce rpath)))
            rsubset #{rpath}]
        (reduce-wtrek grammar new-wtrek (assoc opts :reduced-subset rsubset)))
      (do
;;        (println "******************* no rpath *******************")
        wtrek))))

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

(defn prune-grammar->sorted-ebnf
  [grammar {:keys [wtrek cycle-set] :as ctx}]
  (let [red-grammar (prune-grammar grammar {:wtrek wtrek})
        acyclic-grammar (apply dissoc red-grammar cycle-set)
        rule-order (codegen/check-and-order-rules acyclic-grammar)
        ordered (concat
                  (map #(vector % (get acyclic-grammar %)) rule-order)
                  (select-keys red-grammar cycle-set))]
    (grammar/grammar->ebnf (reverse ordered))))
