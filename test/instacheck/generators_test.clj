(ns instacheck.generators-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check.generators :as gen]
            [instacheck.generators :as igen]))

(deftest freq-test
  (testing "freq"
    (let [gen1 (igen/freq :foo [[0   (gen/return "a")]
                                [100 (gen/return "b")]])
          gen2 (igen/freq :bar [[0   (gen/return "a")]
                                [0   (gen/return "b")]])]

      (testing "all \"b\""
        (is (every? #(= % "b")
                    (take 100 (gen/sample-seq gen1)))))

      (testing " igen/freq with zero weights with runtime throw"
        (is (thrown? Exception
                     (doall (take 100 (gen/sample-seq gen2)))))))))
