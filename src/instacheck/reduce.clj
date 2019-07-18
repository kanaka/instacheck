(ns instacheck.reduce
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [instacheck.util :as util]
            [instacheck.grammar :as grammar]
            [instacheck.weights :as weights]
            [instacheck.codegen :as codegen]))

(def memoized-tree-distances
  (memoize util/tree-distances))
(def memoized-paths-to-leaf
  (memoize grammar/paths-to-leaf))
(def memoized-trek
  (memoize grammar/trek))

;; ---

(defn reduce-wtrek
  "Takes a grammar and wtrek and returns a new reduced wtrek with all
  parent weights reduced to their largest child weight (if all child
  weights are smaller).


  Algorithm/Pseudocode:
    - set pend to contain all weighted nodes in the tree.
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
  [grammar wtrek]
  (loop [wtrek wtrek
         pend (set (filter #(grammar/WEIGHTED (last %))
                           (map (comp pop key) wtrek)))]
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



;; (defn- rule-leaf-immediate-dist
;;   "How many weighted node are crossed from rule-nt to leaf-nt within
;;   the same rule. If leaf-nt occurs multiple times then returns the
;;   shortest depth. If rule-nt and leaf-nt are the same then returns 0."
;;   [grammar rule-nt leaf-nt]
;;   (let [paths (filter #(= rule-nt (first %))
;;                       (grammar/paths-to-leaf grammar leaf-nt))]
;;     (prn :rule-nt rule-nt :leaf-nt leaf-nt :paths paths)
;;     (if (= rule-nt leaf-nt)
;;       0
;;       (apply min
;;              (for [path paths]
;;                (count (filter #(grammar/WEIGHTED %) path)))))))
;;
;; (defn- grammar-distances
;;   [grammar & [start]]
;;   (let [start (or start (:start (meta grammar)))
;;         deps (util/tree-deps grammar)
;;         dists (into
;;                 {} (for [[rule-nt leaf-nts] deps]
;;                      [rule-nt
;;                       (into
;;                         {} (for [leaf-nt leaf-nts]
;;                              [leaf-nt
;;                               (rule-leaf-immedate-dist
;;                                 grammar rule-nt leaf-nt)]))]))]
;;     (util/tree-distances grammar dists start)))
;;
;; (defn- wtrek-dists
;;   [grammar start-nt]
;;   (let [wtrek (grammar/wtrek grammar)]
;;         gdist (grammar-distances grammar start-nt)]
;;   )

(defn- ltrek*
  "Given a grammar and a wtrek, update ltrek with likelihood of
  reaching every node of grammar."
  [grammar wtrek path node ltrek likelihood]
  (let [tag (:tag node)
        ltrek (update ltrek path (fnil + 0) likelihood)]
;;   (prn :path path :tag tag :likelihood likelihood)
    (cond
      (< likelihood 0.001)
      ltrek

      (#{:string :regexp :epsilon} tag)
      ltrek

      (= :nt tag)
      (let [nt (:keyword node)]
        (recur grammar wtrek [nt] (get grammar nt) ltrek likelihood))

      (#{:cat :alt :ord :star :opt} tag)
      (let [child-paths (grammar/children-of-node grammar (conj path tag))
            child-nodes (cond
                          (#{:cat :alt} tag)  (:parsers node)
                          (= :ord tag)        [(:parser1 node) (:parser2 node)]
                          (#{:star :opt} tag) [{:tag :epsilon} (:parser node)])
            total-weight (apply + (map #(get wtrek % 100) child-paths))]
;;        (println "  " :child-paths child-paths)
        (reduce
          (fn [lt [p n]]
            (ltrek* grammar wtrek p n lt
                    (if (= :cat tag)
                      likelihood
                      (* likelihood (/ (get wtrek p) total-weight)))))
          ltrek
          (map vector child-paths child-nodes)))

      (= :plus tag)
      (recur grammar wtrek (conj path :plus 0) (:parser node) ltrek likelihood))))

(defn likelihood-trek
  [grammar wtrek]
  (let [start (:start (meta grammar))]
    (ltrek* grammar wtrek [start] (get grammar start) {} 1)))

(defn terminal-likelihood-trek
  [grammar wtrek]
  (let [tk (grammar/trek grammar)
        ttk (into {} (filter (comp grammar/TERMINAL val) tk))]
    (select-keys (likelihood-trek grammar wtrek) (keys ttk))))

(defn- dtrek*
  "Given a grammar and a wtrek, update dtrek with distance to reach
  every node of grammar."
  [grammar path node dtrek dist]
  (let [tag (:tag node)]
;;   (prn :path path :tag tag :dist dist)
    (cond
      (contains? dtrek path)
      dtrek

      (#{:string :regexp :epsilon} tag)
      (assoc dtrek path dist)

      (= :nt tag)
      (let [nt (:keyword node)]
        (recur grammar [nt] (get grammar nt)
               (assoc dtrek path dist) (inc dist)))

      (#{:cat :alt :ord :star :opt} tag)
      (let [child-paths (grammar/children-of-node grammar (conj path tag))
            child-nodes (cond
                          (#{:cat :alt} tag)  (:parsers node)
                          (= :ord tag)        [(:parser1 node) (:parser2 node)]
                          (#{:star :opt} tag) [{:tag :epsilon} (:parser node)])
            next-dist (if (= (count path) 1) (inc dist) dist)]
;;        (println "  " :child-paths child-paths)
        (reduce
          (fn [dt [p n]]
            (assoc
              (dtrek* grammar p n dt (inc next-dist))
              (conj path tag) next-dist
              p (inc next-dist)))
          (assoc dtrek path dist)
          ;;(assoc dtrek path dist (conj path tag) (inc dist))
          (map vector child-paths child-nodes)))

      (= :plus tag)
      (let [next-dist (if (= (count path) 1) (inc dist) dist)]
        (recur grammar (conj path :plus 0) (:parser node)
               (assoc dtrek
                      path dist
                      (conj path :plus) next-dist) (inc next-dist))))))

(defn distance-trek
  [grammar]
  (let [start (:start (meta grammar))]
    (dtrek* grammar [start] (get grammar start) {} 0)))


(def debug (atom nil))
;; There is a problem that arises when we reduce the weight of
;; a nil-ending path (:start/:opt) to 0. This implies infinite
;; recursion because the remaining 0-ending path is the 1 or more
;; portion. However, because we construct our recursvie generator by
;; slicing a the smallest non-recursive tree for the inner generator,
;; we avoid infinite recursion.

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
        rpath (condp = reduce-mode
                ;; sort by grammar dist, rule dist and then weight
                ;; and choose uniformly from heaviest
                :leaf
                (let [_ (assert (:start (meta grammar)))
                      child-dists (into {} (for [[n cs] (util/tree-deps grammar)]
                                             [n (into {} (for [c cs]
                                                           [c (if (= c n) 0 1)]))]))
                      distances (memoized-tree-distances
                                  grammar (:start (meta grammar)) child-dists)
                      ;; _ (prn :distances distances)
                      grouped (group-by (juxt #(get distances (first %))
                                              #(/ (- (count %) 1) 2)
                                              #(or (get wtrek %) 0))
                                        bigs)
;;                      _ (prn :grouped)
;;                      _ (pprint (sort-by key grouped))
                      leafiest (last (sort-by key grouped))]
                  (reset! debug {:wtr weights-to-reduce
                                 :wtrek wtrek
                                 :grouped grouped
                                 :leafiest leafiest})
                  (when (seq leafiest)
                    (rand-nth (val leafiest))))

                ;; just sort by weight and choose uniformly from
                ;; heaviest
                :leaf1
                (let [grouped (group-by #(or (get wtrek %) 0)
                                        bigs)
;;                      _ (prn :grouped)
;;                      _ (pprint (sort-by key grouped))
                      heaviest (last (sort-by key grouped))]
                  (reset! debug {:wtr weights-to-reduce
                                 :wtrek wtrek
                                 :grouped grouped
                                 :heaviest heaviest})
                  (when (seq heaviest)
                    (rand-nth (val heaviest))))

                ;; sort by weight and choose random weighted
                :leaf2
                (let [grouped (group-by #(or (get wtrek %) 0)
                                        bigs)
;;                      _ (prn :grouped)
;;                      _ (pprint (sort-by key grouped))
                      ]
                  (reset! debug {:wtr weights-to-reduce
                                 :wtrek wtrek
                                 :grouped grouped})
                  (when (seq grouped)
                    (util/weighted-rand-nth (for [[w ps] grouped
                                                  p ps]
                                              [p w]))))

                ;; select the path with highest likelihood
                :lleaf1
                (let [ltk (likelihood-trek grammar wtrek)
                      bigs-ltk (select-keys ltk bigs)]
                  (reset! debug {:wtr weights-to-reduce
                                 :wtrek wtrek
                                 :ltk ltk
                                 :big-ltk bigs-ltk})
                  (when (seq bigs-ltk)
                    (last (keys (sort-by val bigs-ltk)))))

                ;; choose random weighted based on likelihood
                :lleaf2
                (let [ltk (likelihood-trek grammar wtrek)
                      bigs-ltk (select-keys ltk bigs)]
                  (reset! debug {:wtr weights-to-reduce
                                 :wtrek wtrek
                                 :ltk ltk
                                 :big-ltk bigs-ltk})
                  (when (seq bigs-ltk)
                    (util/weighted-rand-nth bigs-ltk)))

                ;; sort by weight and then distance and choose
                ;; uniformly from heaviest
                :dleaf1
                (let [distances (distance-trek grammar)
                      ;; _ (prn :distances distances)
                      grouped (group-by (juxt #(or (get wtrek %) 0)
                                              #(get distances %))
                                        bigs)
;;                      _ (prn :grouped)
;;                      _ (pprint (sort-by key grouped))
                      leafiest (last (sort-by key grouped))]
                  (reset! debug {:wtr weights-to-reduce
                                 :wtrek wtrek
                                 :distances distances
                                 :grouped grouped
                                 :leafiest leafiest})
                  (when (seq leafiest)
                    (rand-nth (val leafiest))))

                ;; sort by weight * distance choose random weighted
                :dleaf2
                (let [distances (distance-trek grammar)
                      grouped (group-by #(or (get wtrek %) 0)
                                        bigs)
;;                      _ (prn :grouped)
;;                      _ (pprint (sort-by key grouped))
                      ]
                  (reset! debug {:wtr weights-to-reduce
                                 :wtrek wtrek
                                 :grouped grouped})
                  (when (seq grouped)
                    (util/weighted-rand-nth (for [[w ps] grouped
                                                  p ps]
                                              [p (* w (get distances p))]))))

                )]
    (prn :rpath rpath :wtrek-w (get wtrek rpath) :wtr-w (get weights-to-reduce rpath))
    (if rpath
      (assoc wtrek rpath (reducer-fn
                           (get wtrek rpath)
                           (get weights-to-reduce rpath)))
      (do (println "******************* no rpath *******************")
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

