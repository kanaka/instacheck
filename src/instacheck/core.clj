(ns instacheck.core
  (:require [clojure.string :as string]
            [clojure.walk :as walk]
            [com.rpl.specter :refer [setval]]
            [instaparse.core :as instaparse]
            [clojure.test.check]
            [clojure.test.check.properties]

            [alandipert.kahn :as kahn]
            [instacheck.util :as util]

            ;; Used to eval generated code.
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.generators :as chuck]
            [instacheck.generators :as igen]

            ;; Convenient to have already loaded for testing
            [clojure.pprint :refer [pprint]]))

(def RULES-PER-FUNC 50)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General utilities

(defn parser->grammar
  "Takes a instaparse parser and returns an instacheck grammar: the
  :grammar value from the parser with a metadata map containing the
  start rule (:start)."
  [parser]
  (with-meta
    (util/remove-key (:grammar parser) :red)
    {:start (:start-production parser)}))

(defn load-grammar
  "Takes an EBNF grammar test string and returns an instacheck
  grammar (via parser->grammar)."
  [ebnf]
  (parser->grammar (instaparse/parser ebnf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create test.check generators for sub-trees of an
;; instaparse grammar rule

(declare gen-ROUTE)

(defn- gen-cat
  "Each value must occur in order."
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (if (= 1 (count (-> tree :parsers)))
      (gen-ROUTE (update-in ctx [:path] conj 0)
                 (-> tree :parsers first) indent)
      (str pre "(gen/tuple\n"
           (string/join
             "\n"
             (for [[idx t] (map-indexed vector (-> tree :parsers))
                   :let [ctx (update-in ctx [:path] conj idx)]]
               (gen-ROUTE ctx t (+ 1 indent))))
           ")"))))


(defn- gen-alt
  "One of the values must occur."
  [{:keys [weights-res weights weights-lookup? path] :as ctx} tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (if (= 1 (count (-> tree :parsers)))
      (gen-ROUTE (update-in ctx [:path] conj 0)
                 (-> tree :parsers first) indent)
      (str pre "(igen/freq [\n"
           (string/join
             "\n"
	     (for [[idx t] (map-indexed vector (-> tree :parsers))
                   :let [path (conj (:path ctx) idx)
                         ctx (assoc ctx :path path)
                         pw (get weights path)
                         weight (if pw pw 100)
                         wcomment (when pw
                                    "    ;; ** adjusted by config ***")]]
               (do
                 (when weights-res
                   (swap! weights-res assoc path weight))
                 (if weights-lookup?
                   (str pre "  [(get weights " path " " weight ")" wcomment "\n"
                        (gen-ROUTE ctx t (+ 2 indent)) "]")
                   (str pre "  [" weight wcomment "\n"
                        (gen-ROUTE ctx t (+ 2 indent)) "]")))))
           "])"))))

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

;; TODO: :hide option
(defn- gen-string
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(gen/return " (pr-str (:string tree)) ")")))

(defn- gen-star
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(gen/vector\n"
         (gen-ROUTE ctx (:parser tree) (+ 1 indent)) ")")))

(defn- gen-plus
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(gen/such-that not-empty (gen/vector\n"
         (gen-ROUTE ctx (:parser tree) (+ 1 indent)) "))")))

(defn- gen-opt
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(gen/one-of [\n"
         pre "  (gen/return \"\")\n"
         (gen-ROUTE ctx (:parser tree) (+ 1 indent)) "])")))

(defn- gen-epsilon
  [ctx tree indent]
  (let [pre (apply str (repeat indent "  "))]
    (str pre "(gen/return \"\")")))

;; TODO: mutual recursion?
;; TODO: :hide option
(defn- gen-nt
  [{:keys [cur-nt] :as ctx} tree indent]
  (let [pre (apply str (repeat indent "  "))
        kw (:keyword tree)
        kw-ns (namespace kw)
        gen-dict (:gen-dict ctx)]
    (str pre (if (= cur-nt kw)
               "inner"
               (if kw-ns
                 (str kw-ns "/" (name kw))
                 (if gen-dict
                   (str "(:" (name kw) " " gen-dict ")")
                   (str "gen-" (name kw))))))))

;;;;;;

(def tag-to-gen
  {:cat     gen-cat
   :alt     gen-alt
   :regexp  gen-regexp
   :string  gen-string
   :star    gen-star
   :plus    gen-plus
   :opt     gen-opt
   :epsilon gen-epsilon
   :nt      gen-nt})

(defn- gen-ROUTE
  [{:keys [debug path] :as ctx} tree indent]
  (let [pre (apply str (repeat indent "  "))
        tag (:tag tree)
        f (get tag-to-gen (:tag tree))
        ;; Update path
        ctx (update-in ctx [:path] conj tag)]
    (assert f (str "No generator found for " tag))
    (str (if debug
           (str pre ";; path: " path "\n")
           "")
         (f ctx tree indent))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create generators for full instaparse grammar including
;; immediately recursive grammars.


(defn- prune-rule-recursion
  "Prune a grammar rule of the recursive parts. Identify the smallest
  optional branches of the rule which are recursive and prune/remove
  them."
  [k tree]
  (let [parent? (fn [t] (util/tree-matches
                          #(= % {:tag :nt :keyword k}) t))]
    (walk/postwalk
      (fn [node]
        (if (and (map? node) (parent? node))
          ;; Prune/rewrite the matches sub-trees
          (condp = (:tag node)
            :alt  {:tag :alt
                   :parsers (filter #(not (parent? %))
                                    (:parsers node))}
            :star {:tag :epsilon}
            :opt  {:tag :epsilon}
            node)
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
             [k (prune-rule-recursion k rule)])))

(defn apply-grammar-updates
  "Replace the generators in the grammar as defined by the list of
  {:path PATH :value VALUE} maps in grammar-updates. Find each :path
  in a grammar and replace it with :value (using specter's setval).
  This allows us to replace generators in the grammar with references
  to other generators (such as real generators from a Clojure
  namespace)."
  [grammar grammar-updates]
  (reduce (fn [g {:keys [path value]}]
            (setval path value g))
          grammar grammar-updates))


;;;;;;

(defn- gen-rule-body
  "Takes a rule name, rule grammar and indent level and returns the
  text of a generator for the rule body."
  [ctx k v indent]
  (let [pre (apply str (repeat indent "  "))
        ctx (assoc ctx :cur-nt k :path [k])]
    (if (util/tree-matches #(= k %) v)
      (str pre "(gen/recursive-gen\n"
           pre "  (fn [inner]\n"
           (gen-ROUTE ctx v (+ 2 indent)) ")\n"
           (gen-ROUTE ctx (prune-rule-recursion k v) (+ 1 indent)) ")")
      (str (gen-ROUTE ctx v indent)))))

(defn- check-and-order-rules
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
  (let [grammar (let [gu (:grammar-updates ctx)]
                  (if (fn? gu)
                    (gu ctx grammar)
                    (apply-grammar-updates grammar gu)))
        ordered-rules (check-and-order-rules grammar)
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
  (let [grammar (let [gu (:grammar-updates ctx)]
                  (if (fn? gu)
                    (gu ctx grammar)
                    (apply-grammar-updates grammar gu)))
        ordered-rules (check-and-order-rules grammar)
        partitioned-rules (map-indexed #(vector %1 %2)
                                       (partition-all RULES-PER-FUNC
                                                      ordered-rules))
        ctx (assoc ctx
                   :weights-lookup? true
                   :gen-dict "gmap")]
    (str
      (string/join
        "\n\n"
        (for [[idx rules] partitioned-rules]
          (str
            "(defn- " function "-part-" idx " [gmap weights]\n"
            "  (let [\n"
            (string/join
              "\n\n"
              (for [k rules
                    :let [v (get grammar k)]]
                (str "        gen-" (name k) "\n"
                     (gen-rule-body ctx k v 4) "\n"
                     "        gmap (assoc gmap " k " gen-" (name k) ")"))) "]\n"
            "    gmap))")))
      (str
        "\n\n"
        "(defn " function " [& [gmap weights]]\n"
        "  (let [gmap (or gmap {})\n"
        (string/join
          "\n"
          (for [[idx _] partitioned-rules]
            (str "        gmap (" function "-part-" idx " gmap weights)"))) "]\n"
        "    gmap))"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generator object/API

(defn grammar->generator-obj
  "Return a a new generator object based on the context and instacheck
  grammar object."
  [{:keys [start] :as ctx} grammar]
  (let [ctx (assoc ctx :function "ephemeral")
        fn-src (grammar->generator-func-source ctx grammar)
        gen-fn (binding [*ns* (create-ns 'instacheck.core)]
                 (load-string fn-src))
        start (or start (:start (meta grammar)))
        gens (gen-fn (:weights ctx))
        gen (gen/fmap util/flatten-text (get gens start))]
    (with-meta
      gen
      {:grammar grammar
       :source fn-src
       :function gen-fn
       :context ctx
       :generators gens
       :cur-start start
       :cur-generator gen})))

(defn update-generator-obj
  "Return a new generator object with runtime properties adjusted
  (those that don't require eval of the function source again). Only
  weights and start rule are supported currently."
  [obj & {:keys [weights start]}]
  (let [{:keys [function context cur-start]} (meta obj)
        start (or start cur-start)
        weights (or weights (:weights context))
        gens (function weights)
        gen (gen/fmap util/flatten-text (get gens start))]
    (with-meta
      gen
      (assoc (meta obj)
             :cur-start start
             :cur-generator gen))))

(defn ebnf-gen
  "Takes an path to an EBNF grammar file and return a test.check
  generator. If the start is specified then this the name of the rule
  to use as the starting rule of the grmmar. If start is not specified
  then the first rule in the grammar file is used as the starting
  rule."
  ([ebnf]
   (ebnf-gen {} ebnf))

  ([ctx ebnf]
   (if (map? ebnf)
     (grammar->generator-obj ctx ebnf)
     (grammar->generator-obj ctx (load-grammar ebnf)))))



(comment
  (println (grammar->generator-function-source {} (load-grammar (slurp "test/recur3.ebnf"))))

  (def ebnf-generator (ebnf-gen (slurp "test/recur3.ebnf")))
  (pprint (gen/sample ebnf-generator 10))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Final grammar utilities

(defn clj-prefix [nsname]
  (str
"(ns " nsname "
  (:require [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.generators :as chuck]
            [instacheck.generators :as igen]
            [instacheck.util :as util]))

"))

(defn grammar->ns
  [ctx grammar]
  (assert (:namespace ctx) ":namespace required in ctx")
  (str (clj-prefix (:namespace ctx)) (grammar->generator-defs-source ctx grammar)))

(comment
  (def ebnf-grammar (load-grammar (slurp "test/recur1.ebnf")))
  (spit "joel/gen.clj" (grammar->ns {:namespace "joel.gen"}
                                    ebnf-grammar))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test execution and parsing utilties

(defn run-check
  "Run quick-check against a generator (gen-to-check) using a check
  function (check-fn) and reporter functon (report-fn). Execution
  options (opts) supported are :iterations (default: 10), :max-size
  (default: 200), and :seed."
  [opts gen-to-check check-fn report-fn]
  (let [{:keys [iterations seed max-size]
	 :or {iterations 10
	      ;;seed 1
	      max-size 200
              }} opts
	p (clojure.test.check.properties/for-all* [gen-to-check] check-fn)]
    (clojure.test.check/quick-check iterations p
                                    :seed seed
                                    :max-size max-size
                                    :reporter-fn report-fn)))

(defn parse
  "Use parser to parse text. On success returns the parsed AST. On
  error, throws an ex-info map containing the :failure (instaparse Failure
  object), :text (the original text), and :location (optional location
  parameter)."
  [parser text & [location]]
  (let [result (instaparse/parse parser text :unhide :all)]
    (when (instaparse/failure? result)
      (throw (ex-info "Parse error" {:failure result
                                     :text text
                                     :location location})))
    result))

(defn parse-weights
  "Use parser to parse a sequence of text description objects {:text
  text :location location}. Returns a weights map with the weights set
  to the number of times that path in the grammar was followed/used
  across all the texts from text-objs."
  [parser text-objs]
  (let [grammar (parser->grammar parser)
	;; Get the full set of zero'd out weights by
	;; calling the def generator but throwing away the
	;; result. The weights are in the context atom.
        ctx {:weights-res (atom {})}
        _ (grammar->generator-defs-source ctx grammar)
        zero-weights (into {} (for [[k v] @(:weights-res ctx)] [k 0]))
        ;; Parse each text string
        results (for [{:keys [text location]} text-objs]
                  (parse parser text location))]
    (merge zero-weights
           (frequencies
             (mapcat #(-> % meta :path-log) results)))))

(defn parse-weights-from-files
  "Wrapper around parse-weights that marshals the text objects from
  a list of file paths."
  [parser files]
  (parse-weights parser (for [f files] {:text (slurp f) :location f})))

