(ns instacheck.core-test
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest testing is]]
            [clojure.test.check.generators :as gen]
            [instacheck.grammar :as g]
            [instacheck.weights :as w]
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

(def ebnf6 "
r1 = 'a' / '<' r1 '>'")


(def g1 (g/load-grammar ebnf1))
(def g2 (g/load-grammar ebnf2))
(def g3 (g/load-grammar ebnf3))
(def g4 (g/load-grammar ebnf4))
(def g5 (g/load-grammar ebnf5))
(def g6 (g/load-grammar ebnf6))

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
            g2w (w/wtrek g2)]
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
      (let [wa4 (w/wtrek g4)
            gen4 (core/ebnf->gen {:weights wa4} g4)
            samps (take 100 (gen/sample-seq gen4))]
        (is (every? #(= "d" %) samps))))

    (testing "non-zero on a single path of an :alt"
      (let [wa4 (w/wtrek g4)
            gen4 (core/ebnf->gen {:weights wa4} g4)
            samps (take 100 (gen/sample-seq gen4))]
        (is (every? #(= "d" %) samps))))

    (testing "self-recursion"
      (let [wa5 (w/wtrek g5)
            gen5 (core/ebnf->gen {:weights wa5} g5)
            samps (take 50 (gen/sample-seq gen5))]
        (is (= 50 (count samps))
            (every? #(re-seq #"<*.*>*" %) samps))))

    (testing "self-recursion containing :ord"
      (let [wa6 (w/wtrek g6)
            gen6 (core/ebnf->gen {:weights wa6} g6)
            samps (take 50 (gen/sample-seq gen6))]
        (is (= 50 (count samps))
            (every? #(re-seq #"<*a>*" %) samps))))))

(deftest grammar->ns-test
  (testing "gramar->ns"
    (let [ns-src (core/grammar->ns {:namespace "foo.bar"} g5)]
      ;;    (println ns-src)
      (is (re-seq #"(?s)\(def gen-r3.*\(gen/return \"a\"\).*\).*\(def gen-r2.*\)"
                  ns-src)))))

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

(deftest parse-wtreks-test
 (testing "parse-wtrek/parse-wtreks"
   (let [p (instacheck.core/load-parser "r1 = 'a' r2*; r2 = 'b' | 'c'")
         data (core/parse-wtreks p [["a" :t1] ["ab" :t2] ["abbc" :t3]])]
     (is (= data
            {:parts
             [{:id :t1,
               :parsed [:r1 "a"],
               :wtrek
               {[:r1 :cat 1 :star 0] 0,
                [:r1 :cat 1 :star nil] 1,
                [:r2 :alt 0] 0,
                [:r2 :alt 1] 0}}
              {:id :t2,
               :parsed [:r1 "a" [:r2 "b"]],
               :wtrek
               {[:r1 :cat 1 :star 0] 1,
                [:r1 :cat 1 :star nil] 0,
                [:r2 :alt 0] 1,
                [:r2 :alt 1] 0}}
              {:id :t3,
               :parsed [:r1 "a" [:r2 "b"] [:r2 "b"] [:r2 "c"]],
               :wtrek
               {[:r1 :cat 1 :star 0] 3,
                [:r1 :cat 1 :star nil] 2,
                [:r2 :alt 0] 2,
                [:r2 :alt 1] 1}}],
             :full-wtrek
             {[:r1 :cat 1 :star nil] 3,
              [:r1 :cat 1 :star 0] 4,
              [:r2 :alt 0] 3,
              [:r2 :alt 1] 1}})))))

(deftest ebnf-sample-seq-test
  (testing "ebnf-sample-seq test"
    (testing "alt within opt"
      (let [samps (take 20 (core/ebnf-sample-seq
                             "r1 = ('a' | 'b' )?"
                             {:weights {[:r1 :opt 0 :alt 1] 0}}))]
        (is (every? #(re-seq #"a*" %) samps))))
    (testing "alt within star"
      (let [samps (take 20 (core/ebnf-sample-seq
                             "r1 = ('a' | 'b' )*"
                             {:weights {[:r1 :star 0 :alt 0] 0}}))]
        (is (every? #(re-seq #"b*" %) samps))))
    (testing "weight with recursion"
      (let [samps (take 50 (core/ebnf-sample-seq
                             "r = '0' | '1' r | '2'"
                             {:weights {[:r :alt 2] 0}}))]
        (is (every? #(not (re-seq #"2" %)) samps))))))

(deftest instacheck
  (testing "instacheck tests"
    (testing "instacheck basics"
      (let [ebnf "root = ('foo' #'[0-9]' ) 'bar' *"
            qc-res (core/instacheck #(<= (count %) 7) ebnf
                                    {:iterations 50})]
        (is (= false (:result qc-res)))
        (is (> (-> qc-res :fail first count) 7))
        (is (:shrunk qc-res))
        (is (= ["foo0barbar"] (-> qc-res :shrunk :smallest)))))

    (testing "instacheck all passing"
      (let [log (atom {:samples []
                       :reports []})
            check-fn (fn [sample]
                       (swap! log update-in [:samples] conj sample)
                       true)
            report-fn (fn [report]
                        (swap! log update-in [:reports] conj report))
            qc-res (core/instacheck check-fn ebnf5 {:iterations 5
                                                    :report-fn report-fn})]
        (is (and (= 5 (-> @log :samples count))
                 (= 6 (-> @log :reports count))))))

    (testing "instacheck with a simple failure case"
      (let [check-fn (fn [sample] (if (re-seq #"<<" sample) false true))
            qc-res (core/instacheck check-fn ebnf5 {:iterations 20})]
        ;;(pprint qc-res)
        (is (get #{"<<0>>" "<<10>>"}
                 (-> qc-res :shrunk :smallest first)))))))

