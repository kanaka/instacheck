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
    (testing "remove propagates"
      (let [p (g/load-parser "r1 = 'a' r2; r2 = 'b' | 'c'")
            g (g/parser->grammar p)
            w {[:r2 :alt 0] 0
               [:r2 :alt 1] 0}]
        (= (r/reduce-wtrek g w)
           {:wtrek w,
            :removed #{[:r2] [:r1] [:r2 :alt]}})))
    (testing "remove rule pointing directly to an nt"
      (let [p (g/load-parser "r1 = r2; r2 = 'b' | 'c'")
            g (g/parser->grammar p)
            w {[:r2 :alt 0] 0
               [:r2 :alt 1] 0}]
        (= (r/reduce-wtrek g w)
           {:wtrek w
            :removed #{[:r2] [:r1] [:r2 :alt]}})))
    (testing "propagate through root NT"
      (let [w (merge w1-all {[:foobar :alt 0] 0
                             [:foobar :alt 1] 0})]
        (is (= (r/reduce-wtrek g1 w)
               {:wtrek (merge w {[:start :alt 1] 0}),
                :removed #{[:foobar :alt] [:foobar]}}))))
    (testing "propagate through intermediate parents and root NT"
      (let [w (merge w1-all {[:foobar :alt 0] 0
                             [:foobar :alt 1 :cat 1 :alt 0] 0
                             [:foobar :alt 1 :cat 1 :alt 1] 0})]
        (is (= (r/reduce-wtrek g1 w)
               {:wtrek (merge w {[:foobar :alt 1] 0
                                 [:start :alt 1] 0})
                :removed #{[:foobar :alt] [:foobar]
                           [:foobar :alt 1 :cat 1 :alt]}}))))
    (testing "propagate through intermediate parents and two root NTs"
      (let [w (merge w1-all {[:foobar :alt 0] 0
                             [:foobar :alt 1 :cat 1 :alt 0] 0
                             [:foobar :alt 1 :cat 1 :alt 1] 0
                             [:start :alt 0] 0})]
        (is (= (r/reduce-wtrek g1 w)
               {:wtrek (merge w {[:foobar :alt 1] 0
                                 [:start :alt 1] 0})
                :removed #{[:foobar :alt] [:foobar]
                           [:foobar :alt 1 :cat 1 :alt]
                           [:start] [:start :alt]}}))))
    (testing "Propagate removal through multiple rule trees with cycles"
      (let [w (merge w3-all {[:r3 :alt 0] 0 [:r3 :alt 1] 0})]
        (is (= (r/reduce-wtrek g3 w)
               {:wtrek (merge w {[:r3 :alt 1 :star nil] 0
                                 [:r3 :alt 1 :star 0] 0
                                 [:r2 :alt 2] 0})
                :removed #{[:r3 :alt 1 :star] [:r3 :alt] [:r3]}})))
      (let [w (merge w3-all {[:r2 :alt 0] 0
                             [:r2 :alt 1] 0
                             [:r2 :alt 2] 0})]
        (is (= (r/reduce-wtrek g3 w)
               {:wtrek (merge w {[:r2 :alt 1 :opt nil] 0
                                 [:r2 :alt 1 :opt 0] 0
                                 [:r1 :cat 1 :star 0 :alt 2] 0})
                :removed #{[:r2] [:r2 :alt] [:r2 :alt 1 :opt]}})))
      ;; Without nil path should not remove r2 nodes
      (let [w (merge w3-all {[:r2 :alt 0] 0
                             [:r2 :alt 1 :opt 0] 0
                             [:r2 :alt 2] 0})]
        (is (= (r/reduce-wtrek g3 w)
               {:wtrek (merge w {})
                :removed #{}})))
      ;; But with nil path should remove r2 nodes as well
      (let [w (merge w3-all {[:r2 :alt 0] 0
                             [:r2 :alt 1 :opt nil] 0
                             [:r2 :alt 1 :opt 0] 0
                             [:r2 :alt 2] 0})]
        (is (= (r/reduce-wtrek g3 w)
               {:wtrek (merge w {[:r2 :alt 1] 0
                                 [:r1 :cat 1 :star 0 :alt 2] 0})
                :removed #{[:r2] [:r2 :alt] [:r2 :alt 1 :opt]}})))
      (let [w (merge w3-all {[:r3 :alt 0] 0
                             [:r3 :alt 1] 0})]
        (is (= (r/reduce-wtrek g3 w)
               {:wtrek (merge w {[:r3 :alt 1 :star nil] 0
                                 [:r3 :alt 1 :star 0] 0
                                 [:r2 :alt 2] 0})
                :removed #{[:r3] [:r3 :alt] [:r3 :alt 1 :star]}})))
      (let [w (merge w3-all {[:r2 :alt 0] 0
                             [:r2 :alt 1] 0
                             [:r3 :alt 0] 0
                             [:r3 :alt 1] 0})]
        (is (= (r/reduce-wtrek g3 w)
               {:wtrek (merge w {[:r1 :cat 1 :star 0 :alt 2] 0
                                 [:r2 :alt 2] 0
                                 [:r2 :alt 1 :opt nil] 0
                                 [:r2 :alt 1 :opt 0] 0
                                 [:r3 :alt 1 :star nil] 0
                                 [:r3 :alt 1 :star 0] 0})
                :removed #{[:r2] [:r2 :alt] [:r2 :alt 1 :opt]
                           [:r3] [:r3 :alt] [:r3 :alt 1 :star]}}))))
    (testing "Propagate with nil paths"
      (let [w (merge w4-all {[:r2 :alt 0] 0})]
        (is (= (r/reduce-wtrek g4 w)
               {:wtrek w
                :removed #{}})))
      (let [w (merge w4-all {[:r2 :alt 0] 0
                             [:r2 :alt 1] 0})]
        (is (= (r/reduce-wtrek g4 w)
               {:wtrek (merge w {[:r1 :alt 1 :opt nil] 0
                                 [:r1 :alt 1 :opt 0] 0
                                 [:r1 :alt 1] 0})
                :removed #{[:r2] [:r2 :alt] [:r1 :alt 1 :opt]}})))
      )))


(deftest reduce-wtrek-with-weights-test
  (testing "reduce-wtrek-with-weights on :alts"
    (let [rwh #(r/reduce-wtrek
                 g1 (r/reduce-wtrek-with-weights
                      g1 %1 %2 :all r/reducer-half))
          rwl #(r/reduce-wtrek
                 g1 (r/reduce-wtrek-with-weights
                      g1 %1 %2 :all (partial r/reducer-ladder [30 10 3 1])))
          rw0 #(r/reduce-wtrek
                 g1 (r/reduce-wtrek-with-weights
                      g1 %1 %2 :all r/reducer-zero))]
      (testing "[:foobar :alt 0] reduced by half"
        (let [r (rwh w1-all {[:foobar :alt 0] 1})]
          (is (= (:wtrek r)
                 (merge w1-all {[:foobar :alt 0] 50})))
          (is (= (:removed r) #{}))))
      (testing "[:foobar :alt 0] reduced by half multiple times"
        (let [rfn (fn [w _] (:wtrek (rwh w {[:foobar :alt 0] 1})))]
          (is (= (get (reduce rfn w1-all (range 2)) [:foobar :alt 0]) 25))
          (is (= (get (reduce rfn w1-all (range 4)) [:foobar :alt 0]) 6))
          (is (= (get (reduce rfn w1-all (range 6)) [:foobar :alt 0]) 1))
          (is (= (get (reduce rfn w1-all (range 7)) [:foobar :alt 0]) 0))
          (is (= (get (reduce rfn w1-all (range 9)) [:foobar :alt 0]) 0))))
      (testing "[:foobar :alt 0] reduced by ladder multiple times"
        (let [rfn (fn [w _] (:wtrek (rwl w {[:foobar :alt 0] 1})))]
          (is (= (get (reduce rfn w1-all (range 1)) [:foobar :alt 0]) 30))
          (is (= (get (reduce rfn w1-all (range 2)) [:foobar :alt 0]) 10))
          (is (= (get (reduce rfn w1-all (range 3)) [:foobar :alt 0]) 3))
          (is (= (get (reduce rfn w1-all (range 4)) [:foobar :alt 0]) 1))
          (is (= (get (reduce rfn w1-all (range 5)) [:foobar :alt 0]) 0))
          (is (= (get (reduce rfn w1-all (range 9)) [:foobar :alt 0]) 0))))
      (testing "[:foobar :alt 0] is 0"
        (let [r (rw0 w1-all {[:foobar :alt 0] 1})]
          (is (= (:wtrek r)
                 (merge w1-all {[:foobar :alt 0] 0})))
          (is (= (:removed r) #{}))))
      (testing "[:foobar :alt 1 :cat 1 :alt 0] is 0"
        (let [r (rw0 w1-all {[:foobar :alt 1 :cat 1 :alt 0] 1})]
          (is (= (:wtrek r)
                 (merge w1-all {[:foobar :alt 1 :cat 1 :alt 0] 0})))
          (is (= (:removed r) #{}))))
      (testing "Propagate 0 to parent"
        (let [r (rw0 w1-all {[:foobar :alt 1 :cat 1 :alt 0] 1
                             [:foobar :alt 1 :cat 1 :alt 1] 1})]
          (is (= (:wtrek r)
                 (merge w1-all {[:foobar :alt 1 :cat 1 :alt 0] 0
                                [:foobar :alt 1 :cat 1 :alt 1] 0
                                [:foobar :alt 1] 0})))
          (is (= (:removed r) #{[:foobar :alt 1 :cat 1 :alt]}))))
      (testing "Propagate 0 to parent in a different rule"
        (let [r (rw0 w1-all {[:foobar :alt 0] 1
                             [:foobar :alt 1] 1})]
          (is (= (:wtrek r)
                 (merge w1-all {[:foobar :alt 0] 0
                                [:foobar :alt 1] 0
                                [:start :alt 1] 0})))
          (is (= (:removed r) #{[:foobar] [:foobar :alt]}))))))

  (testing "reduce-wtrek-with-weights of :alt, :opt, :star nodes"
    (let [rw0 #(r/reduce-wtrek
                 g2 (r/reduce-wtrek-with-weights
                      g2 %1 %2 :all r/reducer-zero))]
      (testing "[:r :cat 1 :opt 0 :alt 0] is 0"
        (let [r (rw0 w2-all {[:r :cat 1 :opt 0 :alt 0] 1})]
          (is (= (:wtrek r)
                 (merge w2-all {[:r :cat 1 :opt 0 :alt 0] 0})))
          (is (= (:removed r) #{}))))
      (testing "Propagate removal through opt to root nt"
        (let [r (rw0 w2-all {[:r :cat 1 :opt 0 :alt 0] 1
                             [:r :cat 1 :opt 0 :alt 1] 1})]
          (is (= (:wtrek r)
                 (merge w2-all {[:r :cat 1 :opt 0 :alt 0] 0
                                [:r :cat 1 :opt 0 :alt 1] 0
                                [:r :cat 1 :opt 0] 0})))
          (is (= (:removed r) #{[:r :cat 1 :opt 0 :alt]})))
        (let [r (rw0 w2-all {[:r :cat 1 :opt nil] 1
                             [:r :cat 1 :opt 0 :alt 0] 1
                             [:r :cat 1 :opt 0 :alt 1] 1})]
          (is (= (:wtrek r)
                 (merge w2-all {[:r :cat 1 :opt 0 :alt 0] 0
                                [:r :cat 1 :opt 0 :alt 1] 0
                                [:r :cat 1 :opt nil] 0
                                [:r :cat 1 :opt 0] 0})))
          (is (= (:removed r) #{[:r]
                                [:r :cat 1 :opt]
                                [:r :cat 1 :opt 0 :alt]})))))))

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
