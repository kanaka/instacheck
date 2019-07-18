(ns instacheck.weights-test
  (:require [clojure.test :refer [deftest testing is]]
            [instacheck.core :as c]
            [instacheck.grammar :as g]
            [instacheck.weights :as w]))

(def ebnf1 "
start = 'qux' | foobar ;
foobar = 'foo' | 'ba' ( 'r' | 'z' ) ;")

(def w1 {[:foobar :alt 0] 0
         [:foobar :alt 1] 0
         [:foobar :alt 1 :cat 1 :alt 0] 0
         [:foobar :alt 1 :cat 1 :alt 1] 0
         [:start :alt 0] 0
         [:start :alt 1] 50})

(def ebnf2 "
foo = bar ;
bar = 'bar' ;
star = '1'* ;
plus = '2'+ ;
opt = '3'?  ;
str = '4' ;
re = #'5';
ep = ''")

(def ebnf3 "
r1 = r2 | r3
r2 = ( r3 | 'm' r3+)
r3 = 'x'")

(def ebnf4 "
x1 = 'a' ( 'b' | 'c' | ('d' / 'e' / 'f' )? )*")

(def ebnf5 "
start = 'qux' (* {:weight 10} *)
      | 'quux'
      | foobar (* {:weight 20} *) ;
foobar = 'foo' (* {:weight 30} *)
       | 'ba' ( 'r' (* {:weight 50} *) |
                'z' (* {:weight 60} *) ) (* {:weight 40} *) ;")

(def ebnf6 "
r = ('a' 'b'? )?")

(def ebnf7 "
r = ('a' 'b'* )*")

(def ebnf8 "
r = 'a'+ 'b'* 'c'?")

(def ebnf9 "
r = 'a' ( 'b' | ( ( 'c' 'd'? )+ | 'e')* )?")

;; grammar loading/conversion

(def g1 (g/load-grammar ebnf1))
(def g3 (g/load-grammar ebnf3))
(def g6 (g/load-grammar ebnf6))
(def p6 (g/grammar->parser g6))
(def g7 (g/load-grammar ebnf7))
(def p7 (g/grammar->parser g7))
(def g8 (g/load-grammar ebnf8))
(def p8 (g/grammar->parser g8))
(def g9 (g/load-grammar ebnf9))
(def p9 (g/grammar->parser g9))

;; weight functions

(deftest removed-node?-test
  (testing "removed-node?"
    (is (= (nil? (w/removed-node? g1 w1 [:foobar :alt]))
           false))
    (is (= (nil? (w/removed-node? g1 w1 [:start :alt]))
           true))
    (is (= (nil? (w/removed-node? g1 w1 [:foobar :alt 1 :cat 1 :alt]))
           false))
    (is (= (nil? (w/removed-node? g1 w1 [:notthere :alt]))
           true))))

(deftest filter-trek-weighted-test
  (testing "filter-trek-weighted"
    (let [m {[:r :alt 0] 100
             [:r :alt 0 :plus 0] 100
             [:r :alt 1 :cat 1 :alt 0] 100
             [:r :alt 1 :cat 1 :alt 0 :opt 0] 100
             [:r :alt 1 :cat 1 :ord 0] 100
             [:r] 100
             [:r :cat 1] 100}]
      (is (= (w/filter-trek-weighted m)
             {[:r :alt 0] 100
              [:r :alt 1 :cat 1 :alt 0] 100
              [:r :alt 1 :cat 1 :alt 0 :opt 0] 100
              [:r :alt 1 :cat 1 :ord 0] 100})))))

(deftest wtrek-test
  (testing "wtrek"
    (is (= (w/wtrek g3)
           {[:r2 :alt 0] 100,
            [:r1 :alt 0] 100,
            [:r1 :alt 1] 100,
            [:r2 :alt 1] 100}))
    (is (= (w/wtrek g3 12)
           {[:r2 :alt 0] 12,
            [:r1 :alt 0] 12,
            [:r1 :alt 1] 12,
            [:r2 :alt 1] 12}))))

(deftest path-log-trek-test
  (testing "path-log-trek"
    (is (= (w/path-log-trek g9 (c/parse p9 "a"))
           {[:r :cat 1 :opt 0 :alt 0] 0,
            [:r] 1,
            [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 0] 0,
            [:r :cat 1 :opt nil] 1,
            [:r :cat 1 :opt 0] 0,
            [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0] 0,
            [:r :cat 0] 1,
            [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0] 0,
            [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 1] 0,
            [:r :cat 1] 1,
            [:r :cat 1 :opt 0 :alt 1] 0,
            [:r :cat 1 :opt 0 :alt 1 :star nil] 0,
            [:r :cat 1 :opt 0 :alt 1 :star 0] 0,
            [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 1 :opt nil] 0,
            [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 1 :opt 0] 0,
            [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 1] 0}))
    (is (= (w/path-log-trek g9 (c/parse p9 "acdccdeeec"))
           {[:r :cat 1 :opt 0 :alt 0] 0,
            [:r] 1,
            [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 0] 4,
            [:r :cat 1 :opt nil] 0,
            [:r :cat 1 :opt 0] 1,
            [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0] 7,
            [:r :cat 0] 1,
            [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0] 6,
            [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 1] 6,
            [:r :cat 1] 1,
            [:r :cat 1 :opt 0 :alt 1] 1,
            [:r :cat 1 :opt 0 :alt 1 :star nil] 9,
            [:r :cat 1 :opt 0 :alt 1 :star 0] 10,
            [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 1 :opt nil] 4,
            [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 1 :opt 0] 2,
            [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 1] 3}))))

(deftest path-log-wtrek-test
  (testing "wtrek, path-log-wtrek"
    (testing "g6"
      (is (= (w/wtrek g6)
             {[:r :opt 0] 100,
              [:r :opt nil] 100,
              [:r :opt 0 :cat 1 :opt 0] 100,
              [:r :opt 0 :cat 1 :opt nil] 100}))
      (is (= (w/path-log-wtrek g6 (c/parse p6 ""))
             {[:r :opt 0] 0,
              [:r :opt nil] 1,
              [:r :opt 0 :cat 1 :opt 0] 0,
              [:r :opt 0 :cat 1 :opt nil] 0}))
      (is (= (w/path-log-wtrek g6 (c/parse p6 "a"))
             {[:r :opt 0] 1,
              [:r :opt nil] 0,
              [:r :opt 0 :cat 1 :opt 0] 0,
              [:r :opt 0 :cat 1 :opt nil] 1}))
      (is (= (w/path-log-wtrek g6 (c/parse p6 "ab"))
             {[:r :opt 0] 1,
              [:r :opt nil] 0,
              [:r :opt 0 :cat 1 :opt 0] 1,
              [:r :opt 0 :cat 1 :opt nil] 0})))

    (testing "g7"
      (is (= (w/wtrek g7)
             {[:r :star 0] 100,
              [:r :star nil] 100,
              [:r :star 0 :cat 1 :star 0] 100,
              [:r :star 0 :cat 1 :star nil] 100}))
      (is (= (w/path-log-wtrek g7 (c/parse p7 ""))
             {[:r :star 0] 0,
              [:r :star nil] 1,
              [:r :star 0 :cat 1 :star 0] 0,
              [:r :star 0 :cat 1 :star nil] 0}))
      (is (= (w/path-log-wtrek g7 (c/parse p7 "a"))
             {[:r :star 0] 1,
              [:r :star nil] 0,
              [:r :star 0 :cat 1 :star 0] 0,
              [:r :star 0 :cat 1 :star nil] 1}))
      (is (= (w/path-log-wtrek g7 (c/parse p7 "ab"))
             {[:r :star 0] 2,
              [:r :star nil] 1,
              [:r :star 0 :cat 1 :star 0] 1,
              [:r :star 0 :cat 1 :star nil] 1}))
      (is (= (w/path-log-wtrek g7 (c/parse p7 "aa"))
             {[:r :star 0] 2,
              [:r :star nil] 1,
              [:r :star 0 :cat 1 :star 0] 0,
              [:r :star 0 :cat 1 :star nil] 2}))
      (is (= (w/path-log-wtrek g7 (c/parse p7 "abb"))
             {[:r :star 0] 3,
              [:r :star nil] 2,
              [:r :star 0 :cat 1 :star 0] 2,
              [:r :star 0 :cat 1 :star nil] 1})))

    (testing "g8"
      (is (= (w/wtrek g8)
             {[:r :cat 1 :star 0] 100,
              [:r :cat 1 :star nil] 100,
              [:r :cat 2 :opt 0] 100,
              [:r :cat 2 :opt nil] 100}))
      (is (= (w/path-log-wtrek g8 (c/parse p8 "a"))
             {[:r :cat 1 :star 0] 0,
              [:r :cat 1 :star nil] 1,
              [:r :cat 2 :opt 0] 0,
              [:r :cat 2 :opt nil] 1}))
      (is (= (w/path-log-wtrek g8 (c/parse p8 "ab"))
             {[:r :cat 1 :star 0] 1,
              [:r :cat 1 :star nil] 1,
              [:r :cat 2 :opt 0] 0,
              [:r :cat 2 :opt nil] 1}))
      (is (= (w/path-log-wtrek g8 (c/parse p8 "abc"))
             {[:r :cat 1 :star 0] 1,
              [:r :cat 1 :star nil] 1,
              [:r :cat 2 :opt 0] 1,
              [:r :cat 2 :opt nil] 0}))
      (is (= (w/path-log-wtrek g8 (c/parse p8 "ac"))
             {[:r :cat 1 :star 0] 0,
              [:r :cat 1 :star nil] 1,
              [:r :cat 2 :opt 0] 1,
              [:r :cat 2 :opt nil] 0}))
      (is (= (w/path-log-wtrek g8 (c/parse p8 "abbc"))
             {[:r :cat 1 :star 0] 2,
              [:r :cat 1 :star nil] 1,
              [:r :cat 2 :opt 0] 1,
              [:r :cat 2 :opt nil] 0})))

    (testing "g9"
      (is (= (w/wtrek g9)
             {[:r :cat 1 :opt 0 :alt 0] 100,
              [:r :cat 1 :opt nil] 100,
              [:r :cat 1 :opt 0] 100,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0] 100,
              [:r :cat 1 :opt 0 :alt 1] 100,
              [:r :cat 1 :opt 0 :alt 1 :star nil] 100,
              [:r :cat 1 :opt 0 :alt 1 :star 0] 100,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 1 :opt 0] 100,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 1 :opt nil] 100,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 1] 100}))
      (is (= (w/path-log-wtrek g9 (c/parse p9 "a"))
             {[:r :cat 1 :opt 0 :alt 0] 0,
              [:r :cat 1 :opt nil] 1,
              [:r :cat 1 :opt 0] 0,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0] 0,
              [:r :cat 1 :opt 0 :alt 1] 0,
              [:r :cat 1 :opt 0 :alt 1 :star nil] 0,
              [:r :cat 1 :opt 0 :alt 1 :star 0] 0,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 1 :opt 0] 0,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 1 :opt nil] 0,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 1] 0}))
      (is (= (w/path-log-wtrek g9 (c/parse p9 "ab"))
             {[:r :cat 1 :opt 0 :alt 0] 1,
              [:r :cat 1 :opt nil] 0,
              [:r :cat 1 :opt 0] 1,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0] 0,
              [:r :cat 1 :opt 0 :alt 1] 0,
              [:r :cat 1 :opt 0 :alt 1 :star nil] 0,
              [:r :cat 1 :opt 0 :alt 1 :star 0] 0,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 1 :opt 0] 0,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 1 :opt nil] 0,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 1] 0}))
      (is (= (w/path-log-wtrek g9 (c/parse p9 "ae"))
             {[:r :cat 1 :opt 0 :alt 0] 0,
              [:r :cat 1 :opt nil] 0,
              [:r :cat 1 :opt 0] 1,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0] 0,
              [:r :cat 1 :opt 0 :alt 1] 1,
              [:r :cat 1 :opt 0 :alt 1 :star nil] 0,
              [:r :cat 1 :opt 0 :alt 1 :star 0] 1,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 1 :opt 0] 0,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 1 :opt nil] 0,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 1] 1}))
      (is (= (w/path-log-wtrek g9 (c/parse p9 "acd"))
             {[:r :cat 1 :opt 0 :alt 0] 0,
              [:r :cat 1 :opt nil] 0,
              [:r :cat 1 :opt 0] 1,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0] 2,
              [:r :cat 1 :opt 0 :alt 1] 1,
              [:r :cat 1 :opt 0 :alt 1 :star nil] 1,
              [:r :cat 1 :opt 0 :alt 1 :star 0] 2,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 1 :opt 0] 1,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 1 :opt nil] 1,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 1] 0}))
      (is (= (w/path-log-wtrek g9 (c/parse p9 "accdcdee"))
             {[:r :cat 1 :opt 0 :alt 0] 0,
              [:r :cat 1 :opt nil] 0,
              [:r :cat 1 :opt 0] 1,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0] 7,
              [:r :cat 1 :opt 0 :alt 1] 1,
              [:r :cat 1 :opt 0 :alt 1 :star nil] 8,
              [:r :cat 1 :opt 0 :alt 1 :star 0] 9,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 1 :opt 0] 2,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 1 :opt nil] 3,
              [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 1] 2})))))

(deftest print-weights-test
  (testing "print-weights test"
    (is (= (with-out-str
             (w/print-weights {[:r1 :alt 1] 20
                               [:r1 :alt 2] 30
                               [:r1 :alt 0] 10}))
           "{[:r1 :alt 0] 10, [:r1 :alt 1] 20, [:r1 :alt 2] 30}\n"))))
