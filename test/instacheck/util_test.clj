(ns instacheck.util-test
  (:require [clojure.test :refer [deftest testing is]]
            [instacheck.util :as u]))

(def ttree {:a [1 2 [:b] {:foo [:c :c]}]
            :b {:bar {:baz [:qux :c]}}
            :c {:foo {:bar [:baz :qux []]}}})


(deftest tree-matches-test
  (testing "tree-matches"
    (testing "whole tree"
      (is (= (u/tree-matches #(= :c %) ttree)
             '(:c :c :c))))
    (testing "tree branches"
      (is (= (u/tree-matches #(= :c %) (:a ttree))
             '(:c :c)))
      (is (= (u/tree-matches #(= :c %) (:b ttree))
             '(:c)))
      (is (= (u/tree-matches #(= :c %) (:c ttree))
             nil)))))

(deftest tree-deps-test
  (testing "tree-deps"
    (is (= (u/tree-deps ttree)
           {:a #{:b :c} :b #{:c} :c #{}}))))

(deftest remove-key-test
  (testing "remove-key"
    (let [tree {:r1 '({:r2 123
                       :r3 [[[[{:abc "zzz" :zzz 789}]]]]
                       :zzz [456]})
                :zzz "abc"}]
      (is (= (u/remove-key tree :zzz)
             {:r1 '({:r2 123
                     :r3 [[[[{:abc "zzz"}]]]]})})))))

(deftest flatten-tree-test
  (testing "flatten-tree"
    (testing "remove empties and concatenate"
      (is (= (u/flatten-text ["foo" "" [[nil "bar"] "baz" ["qux"]]])
             "foobarbazqux")))

    (testing "retain explicit space"
      (is (= (u/flatten-text ["foobar" " " "baz" "qux"])
             "foobar bazqux")))

    (testing "separator"
      (is (= (u/flatten-text ["foo" "" [[nil "bar"] "baz" ["qux"]]] " ")
             "foo bar baz qux")))))
