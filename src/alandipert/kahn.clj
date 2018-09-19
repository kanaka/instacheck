;; Copyright (c) Alan Dipert. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns alandipert.kahn
  (:require [clojure.set :refer [difference union intersection]]))

(defn without
  "Returns set s with x removed."
  [s x] (difference s #{x}))

(defn take-1
  "Returns the pair [element, s'] where s' is set s with element removed."
  [s] {:pre [(not (empty? s))]}
  (let [item (first s)]
    [item (without s item)]))

(defn no-incoming
  "Returns the set of nodes in graph g for which there are no incoming
  edges, where g is a map of nodes to sets of nodes."
  [g]
  (let [nodes (set (keys g))
        have-incoming (apply union (vals g))]
    (difference nodes have-incoming)))

(defn normalize
  "Returns g with empty outgoing edges added for nodes with incoming
  edges only.  Example: {:a #{:b}} => {:a #{:b}, :b #{}}"
  [g]
  (let [have-incoming (apply union (vals g))]
    (reduce #(if (get % %2) % (assoc % %2 #{})) g have-incoming)))

(defn kahn-sort
  "Proposes a topological sort for directed graph g using Kahn's
   algorithm, where g is a map of nodes to sets of nodes. If g is
   cyclic, returns nil."
  ([g]
     (kahn-sort (normalize g) [] (no-incoming g)))
  ([g l s]
     (if (empty? s)
       (when (every? empty? (vals g)) l)
       (let [[n s'] (take-1 s)
             m (g n)
             g' (reduce #(update-in % [n] without %2) g m)]
         (recur g' (conj l n) (union s' (intersection (no-incoming g') m)))))))

(comment
  (def acyclic-g
    {7 #{11 8}
     5 #{11}
     3 #{8 10}
     11 #{2 9}
     8 #{9}})

  (def cyclic-g
    {7 #{11 8}
     5 #{11}
     3 #{8 10}
     11 #{2 9}
     8 #{9}
     2 #{11}}) ;oops, a cycle!

  (kahn-sort acyclic-g) ;=> [3 5 7 8 10 11 2 9]
  (kahn-sort cyclic-g) ;=> nil

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extensions to Alan's code above

(defn no-outgoing
  "Returns the set of nodes in graph g for which there are no outgoing
  edges, where g is a map of nodes to sets of nodes."
  [g]
  (set (map first (filter #(empty? (val %)) (normalize g)))))

(defn cyclic
  [g]
  (let [no-in (no-incoming g)
        no-out (no-outgoing g)]
    (if (and (empty? no-in) (empty? no-out))
      g
      (recur (into {} (map (fn [[k v]]
                             (when (and (not (no-in k))
                                        (intersection v no-out))
                               [k (difference v no-out)]))
                           g))))))

(defn direct-recursive
  [g]
  (let [cycles (cyclic g)]
    (set (map first (filter (fn [[k v]] (contains? v k)) cycles)))))

(defn mutually-recursive
  [g]
  (let [cycles (cyclic g)]
    (into {} (map (fn [[k v]] (if (= #{k} v) nil [k (difference v #{k})]))
                  cycles))))

(defn kahn-sort-throws
  "Proposes a topological sort for directed graph g using Kahn's
   algorithm, where g is a map of nodes to sets of nodes. If g is
   cyclic, throws an exception with the remaining (cyclic) graph."
  ([g]
     (kahn-sort-throws (normalize g) [] (no-incoming g)))
  ([g l s]
     (if (empty? s)
       (if (every? empty? (vals g))
         l
         (throw (ex-info "Cyclic graph" {:graph (cyclic g)})))
       (let [[n s'] (take-1 s)
             m (g n)
             g' (reduce #(update-in % [n] without %2) g m)]
         (recur g' (conj l n) (union s' (intersection (no-incoming g') m)))))))

(defn remove-direct-recursive
  [g]
  (into {} (map (fn [[k v]] [k (disj v k)]) g)))


(comment

  (def cyclic-g {:a #{:c :b}
                 :b #{:e :d}
                 :c #{:a}
                 :d #{:f :a}
                 :h #{:a :i}})
  (cyclic cyclic-g) ;=> {:a #{:c :b}, :b #{:d}, :c #{:a}, :d #{:a}}
  (kahn-sort-throws cyclic-g) ;=> <Exception>
  )
