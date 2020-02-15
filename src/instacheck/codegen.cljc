(ns instacheck.codegen
  (:require [clojure.string :as string]
            [clojure.walk :as walk]

            [alandipert.kahn :as kahn]
            [instacheck.util :as util]
            [instacheck.grammar :as grammar]

            ;; Needed here to evaluated generated generators
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.generators :as chuck]
            [instacheck.generators :as igen]

            ;; Convenient to have already loaded for testing
            [clojure.pprint :refer [pprint]]))

;; Instacheck Clojure code generation

(def RULES-PER-FUNC 50)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create test.check generators for sub-trees of an
;; instaparse grammar rule

(declare gen-ROUTE weight-str)

(defn- gen-cat
  "Each value must occur in order."
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (if (= 1 (count (-> tree :parsers)))
      (gen-ROUTE ctx (-> tree :parsers first) indent)
      (str pre "(gen/tuple\n"
           (string/join
             "\n"
             (for [t (-> tree :parsers)]
               (gen-ROUTE ctx t (+ 1 indent))))
           ")"))))


(defn- gen-alt
  "One of the values must occur."
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (if (= 1 (count (-> tree :parsers)))
      (gen-ROUTE ctx (-> tree :parsers first) indent)
      (str pre "(igen/freq " (conj (:path tree) :alt) " [\n"
           (string/join
             "\n"
             (for [t (-> tree :parsers)]
               (str pre "  [" (weight-str ctx (:path t)) "\n"
                    (gen-ROUTE ctx t (+ 2 indent))
                    "]")))
           "])"))))

(defn- gen-ord
  "One of the values must occur. Like gen-alt with a preference for
  earlier values."
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(igen/freq " (conj (:path tree) :ord) " [\n"
         (string/join
           "\n"
           (for [[adj t] [[1 (-> tree :parser1) ]
                          [0 (-> tree :parser2)]]]
             (str pre "  [" (weight-str ctx (:path t) adj) "\n"
                  (gen-ROUTE ctx t (+ 2 indent))
                  "]")))
         "])")))

(defn- gen-star
  "The value occurs 0 or 1 times."
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(igen/freq " (conj (:path tree) :star) " [\n"
         (string/join
           "\n"
           (for [[idx ind t] [[nil 2 {:tag :epsilon
                                      :path (conj (:path tree) :star nil)}]
                              [0   3 (:parser tree)]]]
             (str pre "  [" (weight-str ctx (:path t)) "\n"
                  (if (= 0 idx)
                    (str pre "    (igen/vector+\n"
                         (gen-ROUTE ctx t (+ ind indent)) ")")
                    (gen-ROUTE ctx t (+ ind indent)))
                  "]")))
         "])")))

(defn- gen-opt
  "The value occurs 0 or 1 times."
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(igen/freq " (conj (:path tree) :opt) " [\n"
         (string/join
           "\n"
           (for [[idx t] [[nil {:tag :epsilon
                                :path (conj (:path tree) :opt nil)}]
                          [0 (-> tree :parser)]]]
             (str pre "  [" (weight-str ctx (:path t)) "\n"
                  (gen-ROUTE ctx t (+ 2 indent))
                  "]")))
         "])")))

(defn- gen-plus
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(igen/vector+\n"
         (gen-ROUTE ctx (:parser tree) (+ 1 indent)) ")")))

(defn- gen-regexp
  "Value must match regexp. For common space value \\s* and \\s+
  generate zero and 1 space respectively."
  [ctx tree indent]
  (let [re (:regexp tree)
        pre (apply str (repeat indent "  "))]
    (cond
      (= (str #"\s*") (str re))
      (str pre "(gen/return \"\")")

      (= (str #"\s+") (str re))
      (str pre "(gen/tuple (gen/return \" \"))")

      :else
      (str pre "(chuck/string-from-regex " (pr-str re) ")"))))

(defn- gen-string
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(gen/return " (pr-str (:string tree)) ")")))

(defn- gen-epsilon
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(gen/return \"\")")))

;; TODO: mutual recursion?
(defn- gen-nt
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))
        kw (:keyword tree)
        kw-ns (namespace kw)
        gen-dict (:gen-dict ctx)]
    (str pre (if (= (first (:path tree)) kw)
               "inner"
               (if kw-ns
                 (str kw-ns "/" (name kw))
                 (if gen-dict
                   (str "(:" (name kw) " " gen-dict ")")
                   (str "gen-" (name kw))))))))

;;;;;;

(def tag-to-gen
  {;; combinators
   :cat     gen-cat
   :alt     gen-alt
   :ord     gen-ord
   :star    gen-star
   :opt     gen-opt
   :plus    gen-plus

   ;; non-terminals/terminal
   :regexp  gen-regexp
   :string  gen-string
   :epsilon gen-epsilon
   :nt      gen-nt})

(defn- gen-ROUTE
  [{:keys [debug] :as ctx} tree indent]
  (let [pre (apply str (repeat indent "  "))
        tag (:tag tree)
        f (get tag-to-gen (:tag tree))]
    (assert f (str "No generator found for " tag))
    (str (if debug
           (str pre ";; path: " (conj (:path tree) tag) "\n")
           "")
         (f ctx tree indent))))

(defn- weight-str
  [{:keys [weights weights-lookup? weights-res]} path & [adjust]]
  (let [pw (get weights path)
        weight (+ (or adjust 0)
                  (if pw pw 100))
        wcomment (when pw "    ;; ** adjusted by config ***")]
    (when weights-res
      (swap! weights-res assoc path weight))
    (if weights-lookup?
      (str "(get w " path " " weight ")" wcomment)
      (str weight wcomment))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create generators for full instaparse grammar including
;; immediately recursive grammars.


(defn- prune-rule-recursion
  "Prune a grammar rule of the recursive parts. Identify the smallest
  optional branches of the rule which are recursive and prune/remove
  them."
  [tree k]
  (let [parent? (fn [t] (util/tree-matches
                          #(and (= :nt (:tag %))
                                (= k (:keyword %))) t))]
    (walk/postwalk
      (fn [node]
        (if (and (map? node) (parent? node))
          ;; Prune/rewrite the matches sub-trees
          (assoc
            (condp = (:tag node)
              :alt  (let [ps (filter #(not (parent? %))
                                     (:parsers node))]
                      (if (seq ps)
                        {:tag :alt
                         :parsers ps}
                        {:tag :epsilon}))
              :ord  (let [p1 (:parser1 node)
                          p1? (parent? p1)
                          p2 (:parser2 node)
                          p2? (parent? p2)]
                      (cond
                        (and p1? p2?) {:tag :epsilon}
                        p1?           p2
                        p2?           p1))
              :star {:tag :epsilon}
              :opt  {:tag :epsilon}
              node)
            :path (:path node))
          node))
      tree)))

(defn- prune-grammar-recursion
  "The test.check gen-recursive generator takes a recursive generator
  and a non-recursive generator as options. For directly recursive
  grammar rules, a version of the rule without the recursion (smallest
  optional part of the rule that recurses is removed) is used for the
  non-recursive part of gen-recursive and the full recursive rule is
  used for the recursive generator to recursive-gen."
  [grammar]
  (into {} (for [[k rule] grammar]
             [k (prune-rule-recursion rule k)])))

;;;;;;

(defn- gen-rule-body
  "Takes a rule name, rule grammar and indent level and returns the
  text of a generator for the rule body."
  [ctx k v indent]
  (when-let [log-fn (:log-fn ctx)] (log-fn "Generating rule body for:" k))
  (let [pre (apply str (repeat indent "  "))
        tree (grammar/walk-grammar-rule v k (fn [p n] (assoc n :path p)))]
    (if (util/tree-matches #(and (= :nt (:tag %))
                                 (= k (:keyword %))) tree)
      (str pre "(gen/recursive-gen\n"
           pre "  (fn [inner]\n"
           (gen-ROUTE ctx tree (+ 2 indent)) ")\n"
           (gen-ROUTE ctx (prune-rule-recursion tree k) (+ 1 indent)) ")")
      (str (gen-ROUTE ctx tree indent)))))

(defn check-and-order-rules
  "Takes an instaparse grammar and returns a sequence of the rule
  names in the order that they can be defined (reverse dependency
  order). If the grammar contains mutually recursive (non-direct)
  rules it will throw an indicating which rules are cyclic."
  [grammar]
  (let [deps (util/tree-deps grammar)
        pruned-deps (kahn/remove-direct-recursive deps)
        _ (assert (empty? (kahn/mutually-recursive pruned-deps))
                  (str "Mutually recursive generators unsupported:"
                       (kahn/mutually-recursive pruned-deps)))
        ordered-rules (reverse (kahn/kahn-sort-throws pruned-deps))]
    ordered-rules))

;; Higher level textual generators

(defn grammar->generator-defs-source
  "Takes an grammar (loaded using load-grammar) and returns the text
  of a namespace with top-level defines (defs) for all the rules. If
  the :start is specified in the ctx then this the name of the rule to
  use as the starting rule of the grammar. If :start is not specified
  then the first rule in the grammar file is used as the starting
  rule. Only the start rule flattens the generated values into a final
  string."
  [{:keys [start] :as ctx} grammar]
  (when-let [log-fn (:log-fn ctx)]
    (log-fn "Ordering rules and checking for mutual recursion"))
  (let [ordered-rules (check-and-order-rules grammar)
        start (or start (:start (meta grammar)))]
    (string/join
      "\n\n"
      (for [k ordered-rules
            :let [v (get grammar k)]]
        (if (= start k)
          (str "(def gen-" (name k) "\n"
               "  (gen/fmap util/flatten-text\n"
               (gen-rule-body ctx k v 2) "))")
          (str "(def gen-" (name k) "\n"
               (gen-rule-body ctx k v 1) ")"))))))

(defn grammar->generator-func-source
  "Takes an grammar (loaded using load-grammar) and returns the text
  of a namespace with a single Clojure function. The function takes
  an optional weights map and returns a map of all the rules as
  generators (indexed by rule-name keyword)."
  [{:keys [function] :as ctx} grammar]
  (assert function "No function name specified")
  (when-let [log-fn (:log-fn ctx)]
    (log-fn "Ordering rules and checking for mutual recursion"))
  (let [ordered-rules (check-and-order-rules grammar)
        partitioned-rules (map-indexed #(vector %1 %2)
                                       (partition-all RULES-PER-FUNC
                                                      ordered-rules))
        ctx (merge ctx {:weights-lookup? true
                        :gen-dict "g"})]
    (str
      (string/join
        "\n\n"
        (for [[idx rules] partitioned-rules]
          (str
            "(defn- " function "-part-" idx " [gmap weights]\n"
            "  (let [g gmap\n"
            "        w weights\n\n"
            (string/join
              "\n\n"
              (for [k rules
                    :let [v (get grammar k)]]
                (str "        gen-" (name k) "\n"
                     (gen-rule-body ctx k v 4) "\n"
                     "        g (assoc g " k " gen-" (name k) ")"))) "]\n"
            "    g))")))
      (str
        "\n\n"
        "(defn " function " [& [gmap weights]]\n"
        "  (let [g (or gmap {})\n"
        "        w weights\n\n"
        (string/join
          "\n"
          (for [[idx _] partitioned-rules]
            (str "        g (" function "-part-" idx " g weights)"))) "]\n"
        "    g))"))))

(defn eval-generator-source
  "Takes a src string containing Clojure code, evaluates it and
  returns the last thing evaluated."
  [src]
  #?(:clj  (binding [*ns* (create-ns 'instacheck.codegen)]
            (load-string src))
     :cljs (throw
             (js/Error.
               "Code eval/load not yet supported in ClojureScript mode"))))

(defn generator-func->generator
  "Takes a generator factory function, a start rule keyword, and an
  optional default weights map. Returns a test.check generator."
  [gen-fn start & [weights]]
  (let [gen-map (gen-fn {} (or weights {}))
        generator (gen/fmap util/flatten-text (get gen-map start))]
    generator))
