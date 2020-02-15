(ns instacheck.codegen-test
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest testing is]]
            [clojure.test.check.generators :as gen]
            [instacheck.grammar :as g]
            [instacheck.codegen :as cg]
            [instacheck.weights :as w]))

(def ebnf1 "
r1 = r2
r2 = r3
r3 = r4
r4 = 'r4'")

(def ebnf2 "
r1 = r2 | r3
r2 = r3 | r4
r3 = r4 / r5
r4 = r5
r5 = 'r5'")

(def ebnf3 "
r1 = r2
r2 = r2 r2 | r3
r3 = r4
r4 = 'r4'")

(def ebnf4 "
r1 = r2
r2 = r3
r3 = r1 | r4
r4 = 'r4'")

(def ebnf5 "
start = 'qux' (* {:weight 10} *)
      | 'quux'
      | foobar (* {:weight 20} *) ;
foobar = 'foo' (* {:weight 30} *)
       | 'ba' ( 'r' (* {:weight 50} *) |
                'z' (* {:weight 60} *) ) (* {:weight 40} *) ;")

(def ebnf6 "
r1 = r2
r2 = #'abc'
r3 = 'def'
r4 = ''
r5 = r1 r2
r6 = r1 | r2
r7 = r1 / r2
r8 = r1*
r9 = r1?
r10 = r1+
r11 = 'a' | r11
r12 = ( 'a' | 'b' )*
r13 = ( 'a' | 'b' )?
r14 = '0' | '1' r14 | '2'
")

(def ebnf7 "
r1 = 'i' #'[0-9]*'")

(def g1 (g/load-grammar ebnf1))
(def g2 (g/load-grammar ebnf2))
(def g3 (g/load-grammar ebnf3))
(def g4 (g/load-grammar ebnf4))
(def g5 (g/load-grammar ebnf5))
(def g6 (g/load-grammar ebnf6))
(def g7 (g/load-grammar ebnf7))

(deftest gen-rule-body-test
  (testing "gen-rule-body"
    (testing "nt"
      (is (= (#'cg/gen-rule-body {} :r1 (:r1 g6) 0)
             "gen-r2")))
    (testing "indent"
      (is (= (#'cg/gen-rule-body {} :r1 (:r1 g6) 2)
             "    gen-r2")))
    (testing "regex"
      (is (= (#'cg/gen-rule-body {} :r2 (:r2 g6) 0)
             "(chuck/string-from-regex #\"abc\")")))
    (testing "string"
      (is (= (#'cg/gen-rule-body {} :r3 (:r3 g6) 0)
             "(gen/return \"def\")")))
    (testing "epsilon"
      (is (= (#'cg/gen-rule-body {} :r4 (:r4 g6) 0)
             "(gen/return \"\")")))
    (testing "cat"
      (is (= (#'cg/gen-rule-body {} :r5 (:r5 g6) 0)
             "(gen/tuple\n  gen-r1\n  gen-r2)")))
    (testing "alt"
      (is (= (#'cg/gen-rule-body {} :r6 (:r6 g6) 0)
             "(igen/freq [:r6 :alt] [\n  [100\n    gen-r1]\n  [100\n    gen-r2]])"))
      (is (= (#'cg/gen-rule-body {:weights-lookup? true} :r6 (:r6 g6) 0)
             "(igen/freq [:r6 :alt] [\n  [(get w [:r6 :alt 0] 100)\n    gen-r1]\n  [(get w [:r6 :alt 1] 100)\n    gen-r2]])")))
    (testing "ord"
      (is (= (#'cg/gen-rule-body {} :r7 (:r7 g6) 0)
             "(igen/freq [:r7 :ord] [\n  [101\n    gen-r1]\n  [100\n    gen-r2]])"))
      (is (= (#'cg/gen-rule-body {:weights-lookup? true} :r7 (:r7 g6) 0)
             "(igen/freq [:r7 :ord] [\n  [(get w [:r7 :ord 0] 101)\n    gen-r1]\n  [(get w [:r7 :ord 1] 100)\n    gen-r2]])")))
    (testing "star"
      (is (= (#'cg/gen-rule-body {} :r8 (:r8 g6) 0)
             "(igen/freq [:r8 :star] [\n  [100\n    (gen/return \"\")]\n  [100\n    (igen/vector+\n      gen-r1)]])"))
      (is (= (#'cg/gen-rule-body {:weights-lookup? true} :r8 (:r8 g6) 0)
             "(igen/freq [:r8 :star] [\n  [(get w [:r8 :star nil] 100)\n    (gen/return \"\")]\n  [(get w [:r8 :star 0] 100)\n    (igen/vector+\n      gen-r1)]])")))
    (testing "opt"
      (is (= (#'cg/gen-rule-body {} :r9 (:r9 g6) 0)
             "(igen/freq [:r9 :opt] [\n  [100\n    (gen/return \"\")]\n  [100\n    gen-r1]])"))
      (is (= (#'cg/gen-rule-body {:weights-lookup? true} :r9 (:r9 g6) 0)
             "(igen/freq [:r9 :opt] [\n  [(get w [:r9 :opt nil] 100)\n    (gen/return \"\")]\n  [(get w [:r9 :opt 0] 100)\n    gen-r1]])")))
    (testing "plus"
      (is (= (#'cg/gen-rule-body {} :r10 (:r10 g6) 0)
             "(igen/vector+\n  gen-r1)")))
    (testing "recursion"
      (is (= (#'cg/gen-rule-body {} :r11 (:r11 g6) 0)
             "(gen/recursive-gen\n  (fn [inner]\n    (igen/freq [:r11 :alt] [\n      [100\n        (gen/return \"a\")]\n      [100\n        inner]]))\n  (gen/return \"a\"))")))
    (testing "alt within star"
      (is (= (#'cg/gen-rule-body {:weights-lookup? true} :r12 (:r12 g6) 0)
             "(igen/freq [:r12 :star] [\n  [(get w [:r12 :star nil] 100)\n    (gen/return \"\")]\n  [(get w [:r12 :star 0] 100)\n    (igen/vector+\n      (igen/freq [:r12 :star 0 :alt] [\n        [(get w [:r12 :star 0 :alt 0] 100)\n          (gen/return \"a\")]\n        [(get w [:r12 :star 0 :alt 1] 100)\n          (gen/return \"b\")]]))]])")))
    (testing "alt within opt"
      (is (= (#'cg/gen-rule-body {:weights-lookup? true} :r13 (:r13 g6) 0)
             "(igen/freq [:r13 :opt] [\n  [(get w [:r13 :opt nil] 100)\n    (gen/return \"\")]\n  [(get w [:r13 :opt 0] 100)\n    (igen/freq [:r13 :opt 0 :alt] [\n      [(get w [:r13 :opt 0 :alt 0] 100)\n        (gen/return \"a\")]\n      [(get w [:r13 :opt 0 :alt 1] 100)\n        (gen/return \"b\")]])]])")))
    (testing "weight indexing with middle recursive element removed"
      (is (= (#'cg/gen-rule-body {:weights-lookup? true} :r14 (:r14 g6) 0)
             "(gen/recursive-gen\n  (fn [inner]\n    (igen/freq [:r14 :alt] [\n      [(get w [:r14 :alt 0] 100)\n        (gen/return \"0\")]\n      [(get w [:r14 :alt 1] 100)\n        (gen/tuple\n          (gen/return \"1\")\n          inner)]\n      [(get w [:r14 :alt 2] 100)\n        (gen/return \"2\")]]))\n  (igen/freq [:r14 :alt] [\n    [(get w [:r14 :alt 0] 100)\n      (gen/return \"0\")]\n    [(get w [:r14 :alt 2] 100)\n      (gen/return \"2\")]]))")))))


(deftest check-and-order-rules-test
  (testing "check-and-order-rules"
    (is (= (cg/check-and-order-rules g1)
           '(:r4 :r3 :r2 :r1)))
    (is (= (cg/check-and-order-rules g2)
           '(:r5 :r4 :r3 :r2 :r1)))
    (is (= (cg/check-and-order-rules g3)
           '(:r4 :r3 :r2 :r1)))

    (testing "mutual recursion"
      (is (thrown? AssertionError (cg/check-and-order-rules g4))))))

(deftest grammar->generator-*-source-test
  (testing "grammar->generator-*-source functions"
    (let [ctx1 {:weights-res (atom {})}
          ctx2 {:weights-res (atom {}) :function "fname"}
          src1 (cg/grammar->generator-defs-source ctx1 g5)
          src2 (cg/grammar->generator-func-source ctx2 g5)
          wa1 @(:weights-res ctx1)
          wa2 @(:weights-res ctx2)
          g5w (w/wtrek g5)]
      (is (re-seq #"(?s)\(def gen-foobar.*\).*\(def gen-start.*\)"
                  src1))
      (is (re-seq #"(?s)\(assoc g :foobar gen-foobar\).*\(assoc g :start gen-start\)"
                  src2))
      (is (= wa1
             wa2
             {[:foobar :alt 0] 100,
              [:foobar :alt 1] 100,
              [:foobar :alt 1 :cat 1 :alt 0] 100,
              [:foobar :alt 1 :cat 1 :alt 1] 100,
              [:start :alt 0] 100,
              [:start :alt 1] 100,
              [:start :alt 2] 100}))
      (is (= (set (keys wa1))
             (set (keys wa2))
             (set (keys g5w)))))))

(deftest eval-generator-source-test
  (testing "eval-generator-source"
    (let [ctx {:function "fname"}
          src (cg/grammar->generator-func-source ctx g5)
          gen-fn (cg/eval-generator-source src)]
      (is gen-fn))))

(deftest generator-func->generator-test
  (testing "generator-func->generator"
    (let [ctx {:function "fname"}
          src (cg/grammar->generator-func-source ctx g5)
          gen-fn (cg/eval-generator-source src)
          gen (cg/generator-func->generator gen-fn :start {})
          samples (take 100 (gen/sample-seq gen))]
      (is (and (= 100 (count samples))
               (every? not-empty samples))))))

(deftest update-grammar-with-builtin-generator-test
  (testing "update-grammar-with-builtin-generator"
    (testing "original grammar"
      (let [ctx {:function "fname"}
            src (cg/grammar->generator-func-source ctx g7)
            gen-fn (cg/eval-generator-source src)
            gen (cg/generator-func->generator gen-fn :r1 {})
            samples (take 100 (gen/sample-seq gen))]
        (is (= 100 (count samples)))
        (is (every? not-empty samples))
        (is (every? #(re-seq #"^i[0-9]*$" %) samples))))

  (testing "update a grammar to use built-in :gen/int generator"
    (let [ctx {:function "fname2"}
          g7-2 (g/assoc-in-grammar
                 g7 [:r1 :cat 1] {:tag :nt :keyword :gen/nat})
          src (cg/grammar->generator-defs-source ctx g7-2)
          gen @(cg/eval-generator-source src)
          samples (take 100 (gen/sample-seq gen))]
      (is (= 100 (count samples)))
      (is (every? not-empty samples))
      (is (every? #(re-seq #"^i[0-9]*$" %) samples))))))

