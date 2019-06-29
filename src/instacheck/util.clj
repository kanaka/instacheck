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
  (let [branch? (some-fn map? sequential?)
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

(defn remove-key
  "Walk a tree removing every key/value where key match k"
  [tree k]
  (postwalk #(if (and (vector? %) (= k (first %))) nil %) tree))


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
