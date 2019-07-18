(ns instacheck.weights
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            
            [instacheck.grammar :as grammar]))

(defn removed-node?
  "Takes path to a grammar node and returns true if all the child
  weights of this node are 0. Only paths ending in :alt, :ord, :opt,
  and :star can have child weights)."
  [grammar weights path]
  (let [children (grammar/children-of-node grammar path)
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
  (into {} (filter (fn [[p v]] (grammar/WEIGHTED (last (pop p)))) trek)))

(defn- expand-nil-edge-paths
  "Internal: takes a trek structure and a weight-fn and returns a new
  trek with nil paths added for each nil-edge node in a trek paths. The
  weight-fn does the actual assoc of the new nil paths and determines
  what their weight value should be."
  [trek weight-fn]
  (let [expand-1 (fn [p]
                   (into
                     [p]
                     (for [idx (keep-indexed
                                 #(when (grammar/NIL-EDGE %2) %1) p)]
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
        full-trek (grammar/trek-grammar grammar (fn [p n] {p 0}))
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
           (grammar/comment-trek grammar :weight))))

(defn path-log-trek
  "Takes a grammar and parse-result parsed using that grammar and
  returns a path-log trek based on the :path-log in parse-result. Note
  that this will return a different set of paths than a normal trek or
  wtrek because it contains \"weights\" for all nodes of the grammar
  and not just for leaf or weighted nodes."
  [grammar parse-result]
  (let [weights (-> parse-result meta :path-log frequencies)
        full-trek (grammar/trek-grammar grammar (fn [p n] {p 0}))
        ;; add NIL-EDGE nil edge paths with nil weight
        sparse-trek (expand-nil-edge-paths
                      full-trek
                      (fn [tk p]
                        (if (grammar/NIL-EDGE (last (pop p)))
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

(defn print-weights [weights]
  (let [sm (sorted-map-by #(compare (str %1) (str %2)))]
    (pprint (into sm weights))))

(defn save-weights [path weights]
  (io/make-parents path)
  (spit path (with-out-str (print-weights weights))))

