(ns instacheck.util
  (:require [clojure.set :refer [union]]
            [clojure.walk :refer [postwalk]]
            [clojure.string :as string]))

(defn pr-err
  [& args]
  (binding [*out* *err*]
    (apply println args)
    (flush)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tree-matches
  "Return seq of pred? matches for any node in the tree."
  [pred? tree]
  (let [branch? (some-fn map? sequential? set?)
        children (fn [n] (if (map? n) (vals n) (seq n)))
        all-nodes (tree-seq branch? children tree)]
    (seq (filter pred? all-nodes))))

(defn tree-deps
  "Takes a structure like {:a tree-a :b tree-b :c tree-c} and returns
  a map like {:a #{:b :c} :b #{:c} :c #{}} which means that :a appears
  in tree-b and tree-c, :b appears in tree-c, but :c does not appear
  in tree-a or tree-b."
  [trees]
  (apply merge-with
         union
         (for [k1 (keys trees)
               [k2 t] trees]
           (if (tree-matches #(= k1 %) t)
             {k2 #{k1}}
             {k2 #{}}))))

#_(defn tree-distances
  [trees]
  (let [deps (tree-deps trees)
        dists (into {} (for [[k v] deps
                            dep v]
                        [[k dep] 1]))]
    (loop [left-ks (keys trees)
           dists dists]
      (let [[k & left-ks] left-ks]
        (if (not k)
          dists
          (let [dists (reduce
                       (fn [[[start end] dist]]
                         )
                       {}
                       dists)]
            (recur left-ks dist)))))))

(defn remove-key
  "Walk a tree removing every key/value where key match k"
  [tree k]
  (postwalk #(if (and (vector? %) (= k (first %))) nil %) tree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tree-distances
  "Applies Djikstra's algorithm to find the shortest paths in trees
  from start key to all the other top-level nodes/keys. Returns a map
  of keys from trees with values that are the distance (hops) from the
  start key to that node/key."
  [trees start]
  (let [;; immediate recursive dist is 0, otherwise 1
        child-dists (into {} (for [[n cs] (tree-deps trees)]
                               [n (into {} (for [c cs]
                                             [c (if (= c n) 0 1)]))]))]
    (loop [pending {start 0}
           all-dists {}]
      (if (seq pending)
        (let [[node ndist] (first (sort-by val pending))
              new-pending (dissoc pending node)
              dist (reduce
                     (fn [a [n d]]
                       (if (contains? all-dists n)
                         a
                         (assoc a n (+ ndist d))))
                     {}
                     (get child-dists node))]
          (recur
            (merge-with min new-pending dist)
            (assoc all-dists node ndist)))
        all-dists))))

(defn weighted-rand-nth
  "Take a sequence of val-weight pairs (can be a map of val to
  weights), chooses a weighted random value and returns [idx val]."
  [vals-weights]
  (let [cumm (map vector
                  (map first vals-weights)
                  (reductions + (map second vals-weights)))
        ridx (rand (-> cumm last last))]
    (some #(when (< ridx (second %)) (first %)) cumm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn flatten-text*
  "Take a tree (sequences hierarchy) and flattens it all the way to
  a single sequence of numbers and strings. Empty values are removed."
  [tree]
  (lazy-seq
    (cond
      (or (number? tree) (string? tree))  (list tree)
      (empty? tree)                       (list)
      :else                               (mapcat flatten-text* tree))))

(defn flatten-text
  "Take a tree (sequences hierarchy) and flattens it all the way to
  a single string (optionally separated by sep). Empty values (but not
  blank strings) are removed."
  [tree & [sep]]
  (string/replace
    (apply str (if sep
                 (interpose sep (flatten-text* tree))
                 (flatten-text* tree)))
    #" +" " "))
