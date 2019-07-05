(ns instacheck.grammar-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as string]
            [instaparse.core :as instaparse]
            [instacheck.core :as c]
            [instacheck.grammar :as g]))

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
(def g2 (g/load-grammar ebnf2))
(def g3 (g/load-grammar ebnf3))
(def g4 (g/load-grammar ebnf4))
(def g5 (g/load-grammar ebnf5))
(def g6 (g/load-grammar ebnf6))
(def p6 (g/grammar->parser g6))
(def g7 (g/load-grammar ebnf7))
(def p7 (g/grammar->parser g7))
(def g8 (g/load-grammar ebnf8))
(def p8 (g/grammar->parser g8))
(def g9 (g/load-grammar ebnf9))
(def p9 (g/grammar->parser g9))

(deftest load-grammar-test
  (testing "load-grammar"
    (testing "g1"
      (is (= g1
             '{:start {:tag :alt
                       :parsers
                       ({:tag :string :string "qux"}
                        {:tag :nt :keyword :foobar})}
               :foobar {:tag :alt
                        :parsers
                        ({:tag :string :string "foo"}
                         {:tag :cat
                          :parsers
                          ({:tag :string :string "ba"}
                           {:tag :alt
                            :parsers
                            ({:tag :string :string "r"}
                             {:tag :string :string "z"})})})}})))
    (testing "g2"
      (is (= (assoc-in g2 [:re :regexp] :DELETED)
             '{:foo {:tag :nt :keyword :bar}
               :bar {:tag :string :string "bar"}
               :star {:tag :star :parser {:tag :string :string "1"}}
               :plus {:tag :plus :parser {:tag :string :string "2"}}
               :opt {:tag :opt :parser {:tag :string :string "3"}}
               :str {:tag :string :string "4"}
               :re {:tag :regexp :regexp :DELETED}
               :ep {:tag :epsilon}})))
    (testing "g4"
      (is (= g4
             '{:x1
               {:tag :cat
                :parsers
                ({:tag :string :string "a"}
                 {:tag :star
                  :parser
                  {:tag :alt
                   :parsers
                   ({:tag :string :string "b"}
                    {:tag :string :string "c"}
                    {:tag :opt
                     :parser
                     {:tag :ord
                      :parser1 {:tag :string :string "d"}
                      :parser2 {:tag :ord
                                :parser1 {:tag :string :string "e"}
                                :parser2 {:tag :string :string "f"}}}})}})}})))
    (testing "g5"
      (is (= g5
             '{:start
               {:tag :alt
                :parsers
                ({:tag :string :string "qux" :comments (" {:weight 10} ")}
                 {:tag :string :string "quux"}
                 {:tag :nt :keyword :foobar :comments (" {:weight 20} ")})}
               :foobar
               {:tag :alt
                :parsers
                ({:tag :string :string "foo" :comments (" {:weight 30} ")}
                 {:tag :cat
                  :comments (" {:weight 40} ")
                  :parsers
                  ({:tag :string :string "ba"}
                   {:tag :alt
                    :parsers
                    ({:tag :string :string "r" :comments (" {:weight 50} ")}
                     {:tag :string :string "z" :comments (" {:weight 60} ")})})})}})))))

(deftest grammar->parser-test
  (testing "grammar->parser"
    (testing "round-tripping between grammar and parser"
      (is (= g2 (-> g2
                    g/grammar->parser
                    g/parser->grammar
                    g/grammar->parser
                    g/parser->grammar)))
      (is (= g9 (-> g9
                    g/grammar->parser
                    g/parser->grammar
                    g/grammar->parser
                    g/parser->grammar))))
    (testing "parsers from scratch"
      (let [ebnf "r1 = r2; r2 = 'abc' | 'def'"
            g {:r1 {:tag :nt :keyword :r2}
               :r2 {:tag :alt :parsers '({:tag :string :string "abc"}
                                         {:tag :string :string "def"})}}
            tk {[:r1] :r2
                [:r2 :alt 0] "abc"
                [:r2 :alt 1] "def"}]
        (is (= (instaparse/parser ebnf)
               (g/grammar->parser g :r1)
               (g/grammar->parser (g/trek->grammar tk) :r1)))))))

(deftest grammar->ebnf-test
  (testing "grammar->ebnf tests"
    (let [ebnf1 "r1 = \"abc\" | #\"def\""
          ebnf2 "r1 = \"abc\" | def\ndef = \"def\""]
      (is (= ebnf1
             (g/grammar->ebnf '{:r1 {:tag :alt
                                     :parsers ({:tag :string :string "abc"}
                                               {:tag :regexp :regexp #"def"})}})))
      (is (= ebnf1
             (g/grammar->ebnf (c/load-grammar ebnf1))))
      (is (= (set (string/split ebnf2 #"\n"))
             (set (string/split (g/grammar->ebnf
                                  (c/load-grammar ebnf2)) #"\n")))))))


;; grammar functions

(deftest get-in-grammar-test
  (testing "get-in-grammar"
    (testing "top rule only"
      (is (= (g/get-in-grammar g1 [:start])
             '{:tag :alt :parsers ({:tag :string :string "qux"}
                                   {:tag :nt :keyword :foobar})})))
    (testing "non-existent top rule"
      (is (= (g/get-in-grammar g1 [:not-there])
             nil)))
    (testing "return whole :alt"
      (is (= (g/get-in-grammar g1 [:start :alt])
             '{:tag :alt :parsers ({:tag :string :string "qux"}
                                   {:tag :nt :keyword :foobar})})))
    (testing "just one branch of :alt"
      (is (= (g/get-in-grammar g1 [:foobar :alt 0])
             {:tag :string :string "foo"})))
    (testing "including :cat, :star and :opt"
      (is (= (g/get-in-grammar g4 [:x1 :cat 0])
             {:tag :string :string "a"}))
      (is (= (g/get-in-grammar g4 [:x1 :cat 1 :star 0 :alt 0])
             {:tag :string :string "b"}))
      (is (= (g/get-in-grammar g4 [:x1 :cat 1 :star 0 :alt 2 :opt 0])
             {:tag :ord,
              :parser1 {:tag :string, :string "d"},
              :parser2 {:tag :ord,
                        :parser1 {:tag :string, :string "e"},
                        :parser2 {:tag :string, :string "f"}}}))
      (is (= (g/get-in-grammar g4 [:x1 :cat 1 :star 0 :alt 2 :opt nil])
             nil))
      (is (= (g/get-in-grammar g4 [:x1 :cat 1 :star 0 :alt 2 :opt 0 :ord 0])
             {:tag :string :string "d"}))
      (is (= (g/get-in-grammar g4 [:x1 :cat 1 :star 0 :alt 2 :opt 0 :ord 1 :ord 0])
             {:tag :string :string "e"})))))

(deftest assoc-and-update-in-grammar-test
  (testing "grammar update functions"
    (testing "assoc-in-grammar"
      (is (= (g/assoc-in-grammar
               g3 [:r2 :alt 1]
               (g/get-in-grammar
                 g3 [:r2 :alt 1 :cat 0]))
             '{:r1 {:tag :alt, :parsers ({:tag :nt, :keyword :r2}
                                         {:tag :nt, :keyword :r3})},
               :r2 {:tag :alt, :parsers ({:tag :nt, :keyword :r3}
                                         {:tag :string, :string "m"})},
               :r3 {:tag :string, :string "x"}})))

    (testing "update-in-grammar"
      (let [g (g/load-grammar "r1 = 'ab' | 'c'")]
        (is (= (g/update-in-grammar
                 g [:r1 :alt 1]
                 #(assoc % :string (str (:string %) "d")))
               '{:r1 {:tag :alt, :parsers ({:tag :string, :string "ab"}
                                           {:tag :string, :string "cd"})}}))))))

(deftest apply-grammar-update-test
  (is (= (g/apply-grammar-update g3 {[:r1] {:tag :nt :keyword :r2}
                                     [:r2 :alt 1] {:tag :string, :string "m"}
                                     [:r3] {:tag :nt :keyword :gen/char-ascii}})
         '{:r1 {:tag :nt, :keyword :r2},
           :r2 {:tag :alt, :parsers ({:tag :nt, :keyword :r3}
                                     {:tag :string, :string "m"})},
           :r3 {:tag :nt,  :keyword :gen/char-ascii}})))

(deftest children-of-node-test
  (testing "children-of-node"
    (testing "Children of an alt node"
      (is (= (g/children-of-node g1 [:foobar :alt])
             '([:foobar :alt 0]
               [:foobar :alt 1]))))
    (testing "Children of an alt node's child (adding the type should be the same behavior)"
      (is (= (g/children-of-node g1 [:foobar :alt 1])
             (g/children-of-node g1 [:foobar :alt 1 :cat])
             '([:foobar :alt 1 :cat 0]
               [:foobar :alt 1 :cat 1]))))
    (testing "Wrong type should nil"
      (is (= (g/children-of-node g1 [:foobar :ord 1])
             nil)))

    (testing "children of a cat node"
      (is (= (g/children-of-node g8 [:r :cat])
             '([:r :cat 0]
               [:r :cat 1]
               [:r :cat 2]))))
    (testing "children of an opt node"
      (is (= (g/children-of-node g8 [:r :cat 2])
             (g/children-of-node g8 [:r :cat 2 :opt])
             '([:r :cat 2 :opt nil]
               [:r :cat 2 :opt 0]))))
    (testing "children of a star node"
      (is (= (g/children-of-node g8 [:r :cat 1])
             (g/children-of-node g8 [:r :cat 1 :star])
             '([:r :cat 1 :star nil]
               [:r :cat 1 :star 0]))))
    (testing "children of a plus node"
      (is (= (g/children-of-node g8 [:r :cat 0])
             (g/children-of-node g8 [:r :cat 0 :plus])
             '([:r :cat 0 :plus 0]))))))


;; trek functions

(deftest trek-test
  (testing "trek, wtrek"
    (is (= (g/trek g1)
           '{[:start :alt 0] "qux"
             [:start :alt 1] :foobar
             [:foobar :alt 0] "foo"
             [:foobar :alt 1 :cat 0] "ba"
             [:foobar :alt 1 :cat 1 :alt 0] "r"
             [:foobar :alt 1 :cat 1 :alt 1] "z"}))
    (is (= (g/wtrek g1 25)
           {[:start :alt 0] 25
            [:start :alt 1] 25
            [:foobar :alt 0] 25
            [:foobar :alt 1] 25
            [:foobar :alt 1 :cat 1 :alt 0] 25
            [:foobar :alt 1 :cat 1 :alt 1] 25}))
    (is (= (dissoc (g/trek g2) [:re])
           '{[:foo] :bar
             [:bar] "bar"
             [:star :star nil] ""
             [:star :star 0] "1"
             [:plus :plus 0] "2"
             [:opt :opt nil] ""
             [:opt :opt 0] "3"
             [:str] "4"
             ;;[:re] #"5"
             [:ep] ""}))
    (is (= (g/trek g4)
           '{[:x1 :cat 0] "a"
             [:x1 :cat 1 :star nil] ""
             [:x1 :cat 1 :star 0 :alt 0] "b"
             [:x1 :cat 1 :star 0 :alt 1] "c"
             [:x1 :cat 1 :star 0 :alt 2 :opt nil] ""
             [:x1 :cat 1 :star 0 :alt 2 :opt 0 :ord 0] "d"
             [:x1 :cat 1 :star 0 :alt 2 :opt 0 :ord 1 :ord 0] "e"
             [:x1 :cat 1 :star 0 :alt 2 :opt 0 :ord 1 :ord 1] "f"}))
    (is (= (g/trek g5)
           '{[:start :alt 0] "qux"
             [:start :alt 1] "quux"
             [:start :alt 2] :foobar
             [:foobar :alt 0] "foo"
             [:foobar :alt 1 :cat 0] "ba"
             [:foobar :alt 1 :cat 1 :alt 0] "r"
             [:foobar :alt 1 :cat 1 :alt 1] "z"}))
    (is (= (g/wtrek g5 35)
           {[:start :alt 0] 10
            [:start :alt 1] 35
            [:start :alt 2] 20
            [:foobar :alt 0] 30
            [:foobar :alt 1] 40
            [:foobar :alt 1 :cat 1 :alt 0] 50
            [:foobar :alt 1 :cat 1 :alt 1] 60}))))

(deftest comment-wtrek-test
  (testing "comment-wtrek, wtrek"
    (is (= (g/comment-wtrek g5)
           {[:start :alt 0] {:weight 10},
            [:start :alt 2] {:weight 20},
            [:foobar :alt 0] {:weight 30},
            [:foobar :alt 1] {:weight 40},
            [:foobar :alt 1 :cat 1 :alt 0] {:weight 50},
            [:foobar :alt 1 :cat 1 :alt 1] {:weight 60}}))
    (is (= (g/wtrek g5)
           {[:start :alt 0] 10,
            [:start :alt 1] 100,
            [:start :alt 2] 20,
            [:foobar :alt 0] 30,
            [:foobar :alt 1] 40,
            [:foobar :alt 1 :cat 1 :alt 0] 50,
            [:foobar :alt 1 :cat 1 :alt 1] 60}))
    (is (= (g/wtrek g5 234)
           {[:start :alt 0] 10,
            [:start :alt 1] 234,
            [:start :alt 2] 20,
            [:foobar :alt 0] 30,
            [:foobar :alt 1] 40,
            [:foobar :alt 1 :cat 1 :alt 0] 50,
            [:foobar :alt 1 :cat 1 :alt 1] 60}))))

(deftest trek->grammar-test
  (testing "trek->grammar"
    (testing "Contruct grammar from trek"
      (is (= (g/trek->grammar {[:r1] :r2
                               [:r2 :alt 0] "abc"
                               [:r2 :alt 1] "def"})
             {:r1 {:tag :nt :keyword :r2}
              :r2 {:tag :alt :parsers '({:tag :string :string "abc"}
                                        {:tag :string :string "def"})}})))

    (testing "Test round-tripping from trek and back to grammar"
      ;; Skip g5 since comments are not roundtripped
      (is (= g1 (g/trek->grammar (g/trek g1))))
      (is (= g2 (g/trek->grammar (g/trek g2))))
      (is (= g3 (g/trek->grammar (g/trek g3))))
      (is (= g4 (g/trek->grammar (g/trek g4))))
      (is (= g6 (g/trek->grammar (g/trek g6))))
      (is (= g7 (g/trek->grammar (g/trek g7))))
      (is (= g8 (g/trek->grammar (g/trek g8))))
      (is (= g9 (g/trek->grammar (g/trek g9)))))))

(deftest paths-to-nt-test
  (testing "paths-to-nt"
    (is (= (g/paths-to-nt g1 :start)
           #{}))
    (is (= (g/paths-to-nt g1 :foobar)
           #{[:start :alt 1]}))
    (is (= (g/paths-to-nt g1 :noththere)
           #{}))
    (is (= (g/paths-to-nt g3 :r1)
           #{}))
    (is (= (g/paths-to-nt g3 :r2)
           #{[:r1 :alt 0]}))
    (is (= (g/paths-to-nt g3 :r3)
           #{[:r1 :alt 1]
             [:r2 :alt 0]
             [:r2 :alt 1 :cat 1 :plus 0]}))))


;; weight functions

(deftest removed-node?-test
  (testing "removed-node?"
    (is (= (nil? (g/removed-node? g1 w1 [:foobar :alt]))
           false))
    (is (= (nil? (g/removed-node? g1 w1 [:start :alt]))
           true))
    (is (= (nil? (g/removed-node? g1 w1 [:foobar :alt 1 :cat 1 :alt]))
           false))
    (is (= (nil? (g/removed-node? g1 w1 [:notthere :alt]))
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
      (is (= (g/filter-trek-weighted m)
             {[:r :alt 0] 100
              [:r :alt 1 :cat 1 :alt 0] 100
              [:r :alt 1 :cat 1 :alt 0 :opt 0] 100
              [:r :alt 1 :cat 1 :ord 0] 100})))))

(deftest wtrek-test
  (testing "wtrek"
    (is (= (g/wtrek g3)
           {[:r2 :alt 0] 100,
            [:r1 :alt 0] 100,
            [:r1 :alt 1] 100,
            [:r2 :alt 1] 100}))
    (is (= (g/wtrek g3 12)
           {[:r2 :alt 0] 12,
            [:r1 :alt 0] 12,
            [:r1 :alt 1] 12,
            [:r2 :alt 1] 12}))))

(deftest path-log-trek-test
  (testing "path-log-trek"
    (is (= (g/path-log-trek g9 (c/parse p9 "a"))
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
    (is (= (g/path-log-trek g9 (c/parse p9 "acdccdeeec"))
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
      (is (= (g/wtrek g6)
             {[:r :opt 0] 100,
              [:r :opt nil] 100,
              [:r :opt 0 :cat 1 :opt 0] 100,
              [:r :opt 0 :cat 1 :opt nil] 100}))
      (is (= (g/path-log-wtrek g6 (c/parse p6 ""))
             {[:r :opt 0] 0,
              [:r :opt nil] 1,
              [:r :opt 0 :cat 1 :opt 0] 0,
              [:r :opt 0 :cat 1 :opt nil] 0}))
      (is (= (g/path-log-wtrek g6 (c/parse p6 "a"))
             {[:r :opt 0] 1,
              [:r :opt nil] 0,
              [:r :opt 0 :cat 1 :opt 0] 0,
              [:r :opt 0 :cat 1 :opt nil] 1}))
      (is (= (g/path-log-wtrek g6 (c/parse p6 "ab"))
             {[:r :opt 0] 1,
              [:r :opt nil] 0,
              [:r :opt 0 :cat 1 :opt 0] 1,
              [:r :opt 0 :cat 1 :opt nil] 0})))

    (testing "g7"
      (is (= (g/wtrek g7)
             {[:r :star 0] 100,
              [:r :star nil] 100,
              [:r :star 0 :cat 1 :star 0] 100,
              [:r :star 0 :cat 1 :star nil] 100}))
      (is (= (g/path-log-wtrek g7 (c/parse p7 ""))
             {[:r :star 0] 0,
              [:r :star nil] 1,
              [:r :star 0 :cat 1 :star 0] 0,
              [:r :star 0 :cat 1 :star nil] 0}))
      (is (= (g/path-log-wtrek g7 (c/parse p7 "a"))
             {[:r :star 0] 1,
              [:r :star nil] 0,
              [:r :star 0 :cat 1 :star 0] 0,
              [:r :star 0 :cat 1 :star nil] 1}))
      (is (= (g/path-log-wtrek g7 (c/parse p7 "ab"))
             {[:r :star 0] 2,
              [:r :star nil] 1,
              [:r :star 0 :cat 1 :star 0] 1,
              [:r :star 0 :cat 1 :star nil] 1}))
      (is (= (g/path-log-wtrek g7 (c/parse p7 "aa"))
             {[:r :star 0] 2,
              [:r :star nil] 1,
              [:r :star 0 :cat 1 :star 0] 0,
              [:r :star 0 :cat 1 :star nil] 2}))
      (is (= (g/path-log-wtrek g7 (c/parse p7 "abb"))
             {[:r :star 0] 3,
              [:r :star nil] 2,
              [:r :star 0 :cat 1 :star 0] 2,
              [:r :star 0 :cat 1 :star nil] 1})))

    (testing "g8"
      (is (= (g/wtrek g8)
             {[:r :cat 1 :star 0] 100,
              [:r :cat 1 :star nil] 100,
              [:r :cat 2 :opt 0] 100,
              [:r :cat 2 :opt nil] 100}))
      (is (= (g/path-log-wtrek g8 (c/parse p8 "a"))
             {[:r :cat 1 :star 0] 0,
              [:r :cat 1 :star nil] 1,
              [:r :cat 2 :opt 0] 0,
              [:r :cat 2 :opt nil] 1}))
      (is (= (g/path-log-wtrek g8 (c/parse p8 "ab"))
             {[:r :cat 1 :star 0] 1,
              [:r :cat 1 :star nil] 1,
              [:r :cat 2 :opt 0] 0,
              [:r :cat 2 :opt nil] 1}))
      (is (= (g/path-log-wtrek g8 (c/parse p8 "abc"))
             {[:r :cat 1 :star 0] 1,
              [:r :cat 1 :star nil] 1,
              [:r :cat 2 :opt 0] 1,
              [:r :cat 2 :opt nil] 0}))
      (is (= (g/path-log-wtrek g8 (c/parse p8 "ac"))
             {[:r :cat 1 :star 0] 0,
              [:r :cat 1 :star nil] 1,
              [:r :cat 2 :opt 0] 1,
              [:r :cat 2 :opt nil] 0}))
      (is (= (g/path-log-wtrek g8 (c/parse p8 "abbc"))
             {[:r :cat 1 :star 0] 2,
              [:r :cat 1 :star nil] 1,
              [:r :cat 2 :opt 0] 1,
              [:r :cat 2 :opt nil] 0})))

    (testing "g9"
      (is (= (g/wtrek g9)
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
      (is (= (g/path-log-wtrek g9 (c/parse p9 "a"))
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
      (is (= (g/path-log-wtrek g9 (c/parse p9 "ab"))
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
      (is (= (g/path-log-wtrek g9 (c/parse p9 "ae"))
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
      (is (= (g/path-log-wtrek g9 (c/parse p9 "acd"))
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
      (is (= (g/path-log-wtrek g9 (c/parse p9 "accdcdee"))
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
             (g/print-weights {[:r1 :alt 1] 20
                               [:r1 :alt 2] 30
                               [:r1 :alt 0] 10}))
           "{[:r1 :alt 0] 10, [:r1 :alt 1] 20, [:r1 :alt 2] 30}\n"))))
