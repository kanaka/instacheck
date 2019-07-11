(ns instacheck.generators
  (:require [clojure.test.check.generators :as gen]))

(defn freq
  "Alternate version of standard frequency generator with the the
  weights sorted so that shrinking is to the higher weights first and
  then the earlier weights among those that are equal (Clojure sort is
  stable and will not re-order equal elements). To support removal of
  generator branches via weights this version also allows all the
  weights to be zero and will throw at runtime rather than
  a definition time."
  [pairs]
  (if (= 0 (reduce + (map first pairs)))
    (gen/fmap
      (fn [g] (throw (Exception.
                       "Invalid call to instacheck.generators/freq with zero weights")))
      (gen/return ""))
    (gen/frequency (sort (comparator (fn [a b] (> (first a) (first b))))
                         pairs))))
(defn vector+
  "Version of standard vector generator with at least one item"
  [gen]
  (gen/let [v (gen/vector gen)
            itm gen]
    (conj v itm)))
