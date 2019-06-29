(ns instacheck.core-test
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest testing is]]
            [clojure.test.check.generators :as gen]
            [instacheck.grammar :as g]
            [instacheck.core :as core]))

(def ebnf1 "
r1 = r2 | r3
r2 = r3 | r4
r3 = r4 | r5
r4 = r5
r5 = 'r5'")

(def ebnf2 "
start = 'qux' (* {:weight 10} *)
      | 'quux'
      | foobar (* {:weight 20} *) ;
foobar = 'foo' (* {:weight 30} *)
       | 'ba' ( 'r' (* {:weight 50} *) |
                'z' (* {:weight 60} *) ) (* {:weight 40} *) ;")

(def ebnf3 "
r = 'a'
  | 'b'
  | 'c'
  | 'd' ;")

(def ebnf4 "
r = 'a' (* {:weight 0} *)
  | 'b' (* {:weight 0} *)
  | 'c' (* {:weight 0} *)
  | 'd' (* {:weight 100} *) ;")

(def ebnf5 "
r1 = ( '<' ( r2 | r3 ) '>' )+ | '<' r1 '>'
r2 = '0' | ( #'[1-9]' r2 )
r3 = 'a' ( 'b' | ( ( 'c' 'd'? )+ | 'e')* )?")


(def g1 (g/load-grammar ebnf1))
(def g2 (g/load-grammar ebnf2))
(def g3 (g/load-grammar ebnf3))
(def g4 (g/load-grammar ebnf4))
(def g5 (g/load-grammar ebnf5))

(deftest ebnf->gen-test
  (testing "ebnf->gen"
    (testing "generator from basic grammar"
      (let [gen1 (core/ebnf->gen {} g1)]
        (is (re-seq #"(?s)g \(assoc g :r5 gen-r5\).*g \(assoc g :r1 gen-r1\)"
                    (:fn-src (meta gen1))))
        (is (= "r5" (first (gen/sample-seq gen1))))))

    (testing "generator from grammar with default weights returned"
      (let [ctx2 {:weights-res (atom {})}
            gen2 (core/ebnf->gen ctx2 g2)
            wa2 @(:weights-res ctx2)
            g2w (g/grammar->weights g2)]
        (is (re-seq #"(?s)g \(assoc g :foobar gen-foobar\).*g \(assoc g :start gen-start\)"
                    (:fn-src (meta gen2))))
        (is (= wa2
               {[:foobar :alt 0] 100,
                [:foobar :alt 1] 100,
                [:foobar :alt 1 :cat 1 :alt 0] 100,
                [:foobar :alt 1 :cat 1 :alt 1] 100,
                [:start :alt 0] 100,
                [:start :alt 1] 100,
                [:start :alt 2] 100}))
        (is (= (set (keys wa2))
               (set (keys g2w))))))

    (testing "ebnf->gen + update-generator-obj with sampling and weight extract"
      (let [wa3A {[:r :alt 0] 0
                  [:r :alt 1] 100
                  [:r :alt 2] 0
                  [:r :alt 3] 0}
            wa3B {[:r :alt 0] 0
                  [:r :alt 1] 0
                  [:r :alt 2] 100
                  [:r :alt 3] 0}
            gen3 (core/ebnf->gen {} g3)
            gen3A (core/update-generator-obj gen3 {:weights wa3A})
            gen3B (core/update-generator-obj gen3 {:weights wa3B})

            sampsA (take 100 (gen/sample-seq gen3A))
            sampsB (take 100 (gen/sample-seq gen3B)) ]
        (is (every? #(= "b" %) sampsA))
        (is (every? #(= "c" %) sampsB))))

    (testing "non-zero on a single path of an :alt"
      (let [wa4 (g/grammar->weights g4)
            gen4 (core/ebnf->gen {:weights wa4} g4)
            samps (take 100 (gen/sample-seq gen4))]
        (is (every? #(= "d" %) samps))))))

(deftest grammar->ns-test
  (testing "gramar->ns"
    (let [ns-src (core/grammar->ns {:namespace "foo.bar"} g5)]
      ;;    (println ns-src)
      (is (re-seq #"(?s)\(def gen-r3.*\(gen/return \"a\"\).*\).*\(def gen-r2.*\)"
                  ns-src)))))

(deftest run-check-test
  (testing "run-check all passing"
    (let [gen5 (core/ebnf->gen {} g5)
          log (atom {:samples []
                     :reports []})
          check-fn (fn [sample]
                     (swap! log update-in [:samples] conj sample)
                     true)
          report-fn (fn [report]
                      (swap! log update-in [:reports] conj report))
          qc-res (core/run-check {:iterations 5} gen5 check-fn report-fn)]
      (is (and (= 5 (-> @log :samples count))
               (= 6 (-> @log :reports count))))))

  (testing "run-check with a simple failure case"
    (let [gen5 (core/ebnf->gen {} g5)
          check-fn (fn [sample] (if (re-seq #"<<" sample) false true))
          report-fn (fn [report] true)
          qc-res (core/run-check {:iterations 20} gen5 check-fn report-fn)]
      ;;(pprint qc-res)
      (is (get #{"<<0>>" "<<10>>"}
               (-> qc-res :shrunk :smallest first))))))

(deftest parse-test
  (testing "error throwing parser"
    (let [p5 (g/grammar->parser g5)]
      (is (core/parse p5 "<0>"))
      (is (thrown? Exception (core/parse p5 "abc")))))

  (testing "generate and parse a bunch of samples with no exceptions"
    (let [p5 (g/grammar->parser g5)
          gen5 (core/ebnf->gen {} g5)
          samps (take 50 (gen/sample-seq gen5))]
      ;;(pprint samps)
      (is (every? #(= \< (first %))
                  samps))
      (is (every? #(and (vector? %)
                        (= :r1 (first %)))
                  (map #(core/parse p5 %) samps))))))
