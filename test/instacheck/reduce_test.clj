(ns instacheck.reduce-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [instacheck.grammar :as g]
            [instacheck.reduce :as r]))

(def ebnf1 "
start = 'qux' | foobar ;
foobar = 'foo' | 'ba' ( 'r' | 'z' ) ;")

(def w1-all {[:foobar :alt 0] 100
             [:foobar :alt 1] 100
             [:foobar :alt 1 :cat 1 :alt 0] 100
             [:foobar :alt 1 :cat 1 :alt 1] 100
             [:start :alt 0] 100
             [:start :alt 1] 100})

(def ebnf2 "
r = 'a' ( 'b' | ( ( 'c' 'd'? )+ | 'e')* )?")

(def w2-all {[:r :cat 1 :opt 0 :alt 0] 100,
             [:r :cat 1 :opt nil] 100,
             [:r :cat 1 :opt 0] 100,
             [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0] 100,
             [:r :cat 1 :opt 0 :alt 1] 100,
             [:r :cat 1 :opt 0 :alt 1 :star nil] 100,
             [:r :cat 1 :opt 0 :alt 1 :star 0] 100,
             [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 1 :opt 0] 100,
             [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 0 :plus 0 :cat 1 :opt nil] 100,
             [:r :cat 1 :opt 0 :alt 1 :star 0 :alt 1] 100})

(def ebnf3 "
r1 = 'a' ( 'b' | 'c' | r2 )*
r2 = 'd' | r2? | r3
r3 = 'e' r1+ | r3*
")

(def w3-all {[:r2 :alt 0] 100,
             [:r1 :cat 1 :star 0 :alt 1] 100,
             [:r2 :alt 2] 100,
             [:r3 :alt 0] 100,
             [:r3 :alt 1 :star 0] 100,
             [:r3 :alt 1 :star nil] 100,
             [:r3 :alt 1] 100,
             [:r1 :cat 1 :star nil] 100,
             [:r1 :cat 1 :star 0] 100,
             [:r2 :alt 1 :opt 0] 100,
             [:r2 :alt 1 :opt nil] 100,
             [:r1 :cat 1 :star 0 :alt 2] 100,
             [:r2 :alt 1] 100,
             [:r1 :cat 1 :star 0 :alt 0] 100})

(def ebnf4 "
r1 = 'a' | r2?
r2 = 'b' | 'c'")

(def w4-all {[:r1 :alt 0] 100,
             [:r1 :alt 1] 100,
             [:r1 :alt 1 :opt nil] 100,
             [:r1 :alt 1 :opt 0] 100,
             [:r2 :alt 0] 100,
             [:r2 :alt 1] 100})


(def ebnf5 "
element = '<h1' (<rS> h1-attr)* '>' (element | 'content')* '</h1>'
        | '<h2' (<rS> h2-attr)* '>' (element | 'content')* '</h2>'
        | '<h3' (<rS> h3-attr)* '>' (element | 'content')* '</h3>'
h1-attr = 'h1-attr'
h2-attr = 'h2-attr'
h3-attr = 'h3-attr'
h4-attr = 'h4-attr'
rS = #'\\s+'")

(def w5 {[:element :alt 0 :cat 1 :star nil] 1
	 [:element :alt 0 :cat 3 :star 0 :alt 1] 1
	 [:element :alt 0 :cat 3 :star 0] 1
	 [:element :alt 0 :cat 3 :star nil] 1
	 [:element :alt 0] 1
	 [:element :alt 1 :cat 1 :star 0] 1
	 [:element :alt 1 :cat 1 :star nil] 1
	 [:element :alt 1 :cat 3 :star 0 :alt 1] 1
	 [:element :alt 1 :cat 3 :star 0] 1
	 [:element :alt 1 :cat 3 :star nil] 1
	 [:element :alt 1] 1 })

(def g1 (g/load-grammar ebnf1))
(def g2 (g/load-grammar ebnf2))
(def g3 (g/load-grammar ebnf3))
(def g4 (g/load-grammar ebnf4))
(def g5 (g/load-grammar ebnf5))

(deftest reduce-wtrek-test
  (testing "reduce-wtrek"
    (testing ":alts 25 returns unchanged wtrek"
      (let [p (g/load-parser "r1 = 'a' r2; r2 = 'b' | 'c'")
            g (g/parser->grammar p)
            w {[:r2 :alt 0] 25
               [:r2 :alt 1] 25}]
        (is (= (r/reduce-wtrek g w)
               w))))
    (testing ":alts 0 throws with no propagation"
      (let [p (g/load-parser "r1 = 'a' r2; r2 = 'b' | 'c'")
            g (g/parser->grammar p)
            w {[:r2 :alt 0] 0
               [:r2 :alt 1] 0}]
        (is (thrown? Exception (r/reduce-wtrek g w)))))
    (testing "propagate through root NT"
      (let [w (merge w1-all {[:foobar :alt 0] 0
                             [:foobar :alt 1] 0})]
        (is (= (r/reduce-wtrek g1 w)
               (merge w {[:start :alt 1] 0})))))
    (testing "propagate through intermediate parents and root NT"
      (let [w (merge w1-all {[:foobar :alt 0] 0
                             [:foobar :alt 1 :cat 1 :alt 0] 0
                             [:foobar :alt 1 :cat 1 :alt 1] 0})]
        (is (= (r/reduce-wtrek g1 w)
               (merge w {[:foobar :alt 1] 0
                         [:start :alt 1] 0})))))
    (testing "propagate 12 through intermediate parents and root NT"
      (let [w (merge w1-all {[:foobar :alt 0] 12
                             [:foobar :alt 1 :cat 1 :alt 0] 12
                             [:foobar :alt 1 :cat 1 :alt 1] 12
                             [:start :alt 0] 12})]
        (is (= (r/reduce-wtrek g1 w)
               (merge w {[:foobar :alt 1] 12
                         [:start :alt 1] 12})))))
    (testing "propagate 0 through root NT causes exception"
      (let [w (merge w1-all {[:foobar :alt 0] 0
                             [:foobar :alt 1 :cat 1 :alt 0] 0
                             [:foobar :alt 1 :cat 1 :alt 1] 0
                             [:start :alt 0] 0})]
        (is (thrown? Exception (r/reduce-wtrek g1 w)))))
    (testing "Propagate removal through multiple rule trees with cycles"
      (let [w (merge w3-all {[:r3 :alt 0] 0
                             [:r3 :alt 1] 0})]
        (is (= (r/reduce-wtrek g3 w)
               (merge w {[:r3 :alt 1 :star nil] 0
                         [:r3 :alt 1 :star 0] 0
                         [:r2 :alt 2] 0}))))
      (let [w (merge w3-all {[:r2 :alt 0] 0
                             [:r2 :alt 1] 0
                             [:r2 :alt 2] 0})]
        (is (= (r/reduce-wtrek g3 w)
               (merge w {[:r2 :alt 1 :opt nil] 0
                         [:r2 :alt 1 :opt 0] 0
                         [:r1 :cat 1 :star 0 :alt 2] 0})))))
      (testing "Without nil path should not remove r2 nodes"
        (let [w (merge w3-all {[:r2 :alt 0] 0
                               [:r2 :alt 1 :opt 0] 0
                               [:r2 :alt 2] 0})]
          (is (= (r/reduce-wtrek g3 w)
                 w))))
      (testing "But with nil path should remove r2 nodes as well"
        (let [w (merge w3-all {[:r2 :alt 0] 0
                               [:r2 :alt 1 :opt nil] 0
                               [:r2 :alt 1 :opt 0] 0
                               [:r2 :alt 2] 0})]
          (is (= (r/reduce-wtrek g3 w)
                 (merge w {[:r2 :alt 1] 0
                           [:r1 :cat 1 :star 0 :alt 2] 0})))))
      (let [w (merge w3-all {[:r3 :alt 0] 0
                             [:r3 :alt 1] 0})]
        (is (= (r/reduce-wtrek g3 w)
               (merge w {[:r3 :alt 1 :star nil] 0
                         [:r3 :alt 1 :star 0] 0
                         [:r2 :alt 2] 0}))))
      (let [w (merge w3-all {[:r2 :alt 0] 0
                             [:r2 :alt 1] 0
                             [:r3 :alt 0] 0
                             [:r3 :alt 1] 0})]
        (is (= (r/reduce-wtrek g3 w)
               (merge w {[:r1 :cat 1 :star 0 :alt 2] 0
                         [:r2 :alt 2] 0
                         [:r2 :alt 1 :opt nil] 0
                         [:r2 :alt 1 :opt 0] 0
                         [:r3 :alt 1 :star nil] 0
                         [:r3 :alt 1 :star 0] 0})))))
    (testing "Propagate with nil paths"
      (let [w (merge w4-all {[:r2 :alt 0] 0})]
        (is (= (r/reduce-wtrek g4 w)
               w)))
      (let [w (merge w4-all {[:r2 :alt 0] 0
                             [:r2 :alt 1] 0})]
        (is (= (r/reduce-wtrek g4 w)
               (merge w {[:r1 :alt 1 :opt nil] 0
                         [:r1 :alt 1 :opt 0] 0
                         [:r1 :alt 1] 0}))))))

(deftest reduce-wtrek-with-weights-test
  ;; TODO: reduce-wtrek-with-weights other modes
  (testing "reduce-wtrek-with-weights on :alts"
    (let [rwh #(r/reduce-wtrek
                 g1 (r/reduce-wtrek-with-weights
                      g1 %1 %2 :dleaf2 r/reducer-half))
          rwl #(r/reduce-wtrek
                 g1 (r/reduce-wtrek-with-weights
                      g1 %1 %2 :dleaf2 (partial r/reducer-ladder [30 10 3 1])))
          rw0 #(r/reduce-wtrek
                 g1 (r/reduce-wtrek-with-weights
                      g1 %1 %2 :dleaf2 r/reducer-zero))]
      (testing "[:foobar :alt 0] reduced by half"
        (let [w (rwh w1-all {[:foobar :alt 0] 1})]
          (is (= w
                 (merge w1-all {[:foobar :alt 0] 50})))))
      (testing "[:foobar :alt 0] reduced by half multiple times"
        (let [rfn (fn [w _] (rwh w {[:foobar :alt 0] 1}))]
          (is (= (get (reduce rfn w1-all (range 2)) [:foobar :alt 0]) 25))
          (is (= (get (reduce rfn w1-all (range 4)) [:foobar :alt 0]) 6))
          (is (= (get (reduce rfn w1-all (range 6)) [:foobar :alt 0]) 1))
          (is (= (get (reduce rfn w1-all (range 7)) [:foobar :alt 0]) 0))
          (is (= (get (reduce rfn w1-all (range 9)) [:foobar :alt 0]) 0))))
      (testing "[:foobar :alt 0] reduced by ladder multiple times"
        (let [rfn (fn [w _] (rwl w {[:foobar :alt 0] 1}))]
          (is (= (get (reduce rfn w1-all (range 1)) [:foobar :alt 0]) 30))
          (is (= (get (reduce rfn w1-all (range 2)) [:foobar :alt 0]) 10))
          (is (= (get (reduce rfn w1-all (range 3)) [:foobar :alt 0]) 3))
          (is (= (get (reduce rfn w1-all (range 4)) [:foobar :alt 0]) 1))
          (is (= (get (reduce rfn w1-all (range 5)) [:foobar :alt 0]) 0))
          (is (= (get (reduce rfn w1-all (range 9)) [:foobar :alt 0]) 0))))
      (testing "[:foobar :alt 0] is 0"
        (let [w (rw0 w1-all {[:foobar :alt 0] 1})]
          (is (= w (merge w1-all {[:foobar :alt 0] 0})))))
      (testing "[:foobar :alt 1 :cat 1 :alt 0] is 0"
        (let [w (rw0 w1-all {[:foobar :alt 1 :cat 1 :alt 0] 1})]
          (is (= w (merge w1-all {[:foobar :alt 1 :cat 1 :alt 0] 0})))))
      (testing "Propagate 0 to parent"
        (let [w (rw0 w1-all {[:foobar :alt 1 :cat 1 :alt 0] 1})
              w (rw0 w      {[:foobar :alt 1 :cat 1 :alt 1] 1})]
          (is (= w (merge w1-all {[:foobar :alt 1 :cat 1 :alt 0] 0
                                  [:foobar :alt 1 :cat 1 :alt 1] 0
                                  [:foobar :alt 1] 0})))))
      (testing "Propagate 0 to parent in a different rule"
        (let [w (rw0 w1-all {[:foobar :alt 0] 1})
              w (rw0 w      {[:foobar :alt 1] 1})]
          (is (= w (merge w1-all {[:foobar :alt 0] 0
                                  [:foobar :alt 1] 0
                                  [:start :alt 1] 0})))))))

  (testing "reduce-wtrek-with-weights of :alt, :opt, :star nodes"
    (let [rw0 #(r/reduce-wtrek
                 g2 (r/reduce-wtrek-with-weights
                      g2 %1 %2 :dleaf2 r/reducer-zero))]
      (testing "[:r :cat 1 :opt 0 :alt 0] is 0"
        (let [w (rw0 w2-all {[:r :cat 1 :opt 0 :alt 0] 1})]
          (is (= w
                 (merge w2-all {[:r :cat 1 :opt 0 :alt 0] 0})))))
      (testing "Propagate removal through opt to root nt"
        (let [w (rw0 w2-all {[:r :cat 1 :opt 0 :alt 0] 1})
              w (rw0 w      {[:r :cat 1 :opt 0 :alt 1] 1})]
          (is (= w (merge w2-all {[:r :cat 1 :opt 0 :alt 0] 0
                                  [:r :cat 1 :opt 0 :alt 1] 0
                                  [:r :cat 1 :opt 0] 0}))))
        (is (thrown? Exception
                     (-> w2-all
                         (rw0 {[:r :cat 1 :opt nil] 1})
                         (rw0 {[:r :cat 1 :opt 0 :alt 0] 1})
                         (rw0 {[:r :cat 1 :opt 0 :alt 1] 1}))))))))

(defn- ebnf-set
  [grammar]
  (set (string/split (g/grammar->ebnf grammar) #"\n")))

(deftest prune-grammar-test
  (testing "prune-grammar test"
    (testing "empty options -> same grammar"
      (is (= (r/prune-grammar (with-meta g5 {}) {})
             g5)))
    (testing "h2-attr start option -> only :h2-attr"
      (is (= (r/prune-grammar g5 {:start :h2-attr})
             {:h2-attr {:tag :string :string "h2-attr"}})))
    (testing "wtrek option -> only :element, :h2-attr, :rS"
      (is (= (set (keys (r/prune-grammar g5 {:wtrek w5})))
             #{:element :h2-attr :rS})))

  (testing "prune-grammar to ebnf test"
    (testing "wtrek option -> only :element, :h2-attr, :rS"
      (let [ebnf "
element = '<h1' '>' 'content'* '</h1>' | '<h2' (<rS> h2-attr)* '>' 'content'* '</h2>'
h2-attr = 'h2-attr'
rS = #'\\s+'"]
        (is (= (ebnf-set (r/prune-grammar g5 {:wtrek w5}))
               (ebnf-set (g/load-grammar ebnf))))))

    (testing "wtrek and h2-attr start option -> only :h2-attr, no :rS"
      (let [ebnf "h2-attr = 'h2-attr'"]
        (is (= (ebnf-set (r/prune-grammar g5 {:wtrek w5 :start :h2-attr}))
               (ebnf-set (g/load-grammar ebnf)))))))))
