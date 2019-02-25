(ns instacheck.generators
  (:require [clojure.test.check.generators :as gen]))

(defn freq
  "Version of standard frequency generator with the the weights sorted
  so that shrinking is to the higher weights first and then the earlier
  weights among those that are equal."
  [pairs]
  (gen/frequency (sort (comparator (fn [a b] (> (first a) (first b)))) pairs)))
