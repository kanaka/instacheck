(ns instacheck.core
  (:require [clojure.test.check]
            [clojure.test.check.properties]
            [clojure.test.check.generators :as gen]

            [instacheck.grammar :as grammar]
            [instacheck.weights :as weights]
            [instacheck.codegen :as codegen]
            [instacheck.parse :as parse]

            ;; Convenient to have already loaded for testing
            [clojure.pprint :refer [pprint]]))

;; Make some common definitions available from core
(def load-parser       grammar/load-parser)
(def load-grammar      grammar/load-grammar)
(def parser->grammar   grammar/parser->grammar)
(def grammar->parser   grammar/grammar->parser)
(def get-in-grammar    grammar/get-in-grammar)
(def update-in-grammar grammar/update-in-grammar)
(def assoc-in-grammar  grammar/assoc-in-grammar)
(def trek              grammar/trek)
(def wtrek             weights/wtrek)
(def path-log-wtrek    weights/path-log-wtrek)
(def save-weights      weights/save-weights)
(def parse             parse/parse)

(def grammar->generator-func-source codegen/grammar->generator-func-source)
(def grammar->generator-defs-source codegen/grammar->generator-defs-source)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generator object/API

(defn grammar->generator-obj
  "Return a a new generator object based on the context and instacheck
  grammar object."
  [{:keys [start] :as ctx} grammar]
  (let [ctx (assoc ctx :function "ephemeral")
        start (or start (:start (meta grammar)))
        fn-src (codegen/grammar->generator-func-source ctx grammar)
        gen-fn (codegen/eval-generator-source fn-src)
        gen (codegen/generator-func->generator gen-fn start (:weights ctx))]
    (with-meta
      gen
      {:grammar grammar
       :ctx ctx
       :fn-src fn-src
       :gen-fn gen-fn
       :cur-start start
       :cur-generator gen})))

(defn update-generator-obj
  "Return a new generator object with runtime properties adjusted
  (those that don't require eval of the function source again). Only
  weights and start rule are supported currently."
  [obj {:keys [weights start]}]
  (let [{:keys [gen-fn ctx cur-start]} (meta obj)
        ctx (assoc ctx :weights (or weights (:weights ctx)))
        start (or start cur-start)
        gen (codegen/generator-func->generator gen-fn start (:weights ctx))]
    (with-meta
      gen
      (merge (meta obj)
             {:ctx (assoc ctx :weights weights)
              :cur-start start
              :cur-generator gen}))))

(defn ebnf->gen
  "Takes an EBNF text string, grammar, parser, or file and returns
  a test.check generator. If the start is specified then this the name
  of the rule to use as the starting rule of the grmmar. If start is
  not specified then the first rule in the grammar file is used as the
  starting rule."
  ([ebnf]
   (ebnf->gen {} ebnf))

  ([ctx ebnf]
   (grammar->generator-obj
     ctx
     (if (not (map? ebnf))
       (grammar/load-grammar ebnf)      ;; Text string or file
       (if (:grammar ebnf)
         (grammar/parser->grammar ebnf) ;; Parser
         ebnf)))))                      ;; Assume grammar

(comment
  (println (grammar->generator-function-source {} (grammar/load-grammar (slurp "test/recur3.ebnf"))))

  (def ebnf-generator (ebnf->gen (slurp "test/recur3.ebnf")))
  (pprint (gen/sample ebnf-generator 10))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Final grammar utilities

(defn- clj-prefix [nsname & [extra-requires]]
  (str
"(ns " nsname "
  (:require [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.generators :as chuck]
            [instacheck.generators :as igen]
            " extra-requires "
            [instacheck.util :as util]))

;; Generated by instacheck

"))

(defn grammar->ns
  "Translate a grammar to namespaced Clojure file. Optional third
  argument specifies a list of additional namespace requires to add to
  the ns definition. :namespace must be specified in ctx with the
  namespace name to use (e.g. 'myproj.generators'). If :function is
  specified in ctx then a single generator factory function will be
  generated in the namespace with the value of :function as the name,
  otherwise each generator will be defined as a separate function."
  [ctx grammar & [extra-requires]]
  (assert (:namespace ctx) ":namespace required in ctx")
  (str (clj-prefix (:namespace ctx) extra-requires)
       (if (:function ctx)
         (codegen/grammar->generator-func-source ctx grammar)
         (codegen/grammar->generator-defs-source ctx grammar))))

(comment
  (def ebnf-grammar (grammar/load-grammar (slurp "test/recur1.ebnf")))
  (spit "joel/gen.clj" (grammar->ns {:namespace "joel.gen"}
                                    ebnf-grammar))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing

(defn parse-wtrek
  "Uses parser to parse a text string and returns a map with the
  parsed tree (:parsed) and a path-log wtrek from the parse (:wtrek).
  The wtrek weights are set to the number of times each path in the
  grammar was followed/used to parse the text."
  [parser text & [id]]
  (let [grammar (grammar/parser->grammar parser)
        parsed (parse parser text)
        wtrek (weights/path-log-wtrek grammar parsed)]
    {:id id
     :parsed parsed
     :wtrek wtrek}))

(defn parse-wtreks
  "Takes a sequence of [text id] pairs and uses parse-wtrek to
  generator the following structure:

      {:parts [{:id ID
                :parsed PARSED
                :wtrek  {PATH WEIGHT ...}}
                ...]
       :full-wtrek {PATH WEIGHT ...}}

  The :parts value is a vector of the parse-wtrek result for each
  texts-ids. The :full-wtrek has merged weight counts from all :parts
  :wtrek values."
  [parser texts-ids]
  (let [parts (vec (for [[text id] texts-ids]
                     (parse-wtrek parser text id)))]
    {:parts parts
     :full-wtrek (apply merge-with + (map :wtrek parts))}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sample generator and test execution

(defn ebnf-sample-seq
  "Returns an infinite sequence of generated values based on the ebnf
  definition. Like test.check/sample-seq but takes an ebnf grammar
  definition and uses an optional opts map to specify weights and
  max-size."
  [ebnf & [opts]]
  (let [{:keys [weights max-size]} opts
        gen (ebnf->gen {:weights weights} ebnf)]
    (if max-size
      (gen/sample-seq gen max-size)
      (gen/sample-seq gen))))

(defn ebnf-generate
  "Returns a single sample value from the generator. Like
  test.check/generate but but takes an ebnf grammar definition and
  uses an optional opts map to specify weights, size and seed."
  [ebnf & [opts]]
  (let [{:keys [weights size seed]} opts
        gen (ebnf->gen {:weights weights} ebnf)]
    (cond
      (and size seed) (gen/generate gen size seed)
      size            (gen/generate gen size)
      :else           (gen/generate gen))))

(defn instacheck
  "Instacheck a function with randomly generated test cases. The
  check-fn will be called repeatedly with random strings that are
  generated based on the ebnf-or-gen. The ebnf-or-gen parameter is
  either something that ebnf->gen can turn into a generator (EBNF
  string, grammar, parser) or a plain test.check generator. The
  check-fn should perform a test based on the string and return a nil
  or false if fails the test or any other value to indicate that it
  passes the test. If a failure is detected then the shrinking process
  will performed. The return value is the quick-check return status.

  The optional opts parameter can be used to specify the following
  quick-check options:
    - iterations: number of iterations to check before success (default: 10)
    - max-size:   maximum size for all internal generators (default: 200)
    - seed:       starting seed for generating test cases
    - report-fn:  function to call with a report after each iteration

  If the ebnf-or-gen parameter is not already a generator, then the
  opts parameter can also contain a weights configuration that will be
  used with ebnf->gen to create the generator for use with
  quick-check."
  [check-fn ebnf-or-gen & [opts]]
  (let [{:keys [iterations max-size report-fn seed weights]
         :or {iterations 10
              max-size 200
              report-fn (fn [& args] true)}} opts
        gen-to-check (if (= (type ebnf-or-gen)
                            clojure.test.check.generators.Generator)
                       ebnf-or-gen
                       (ebnf->gen {:weights weights} ebnf-or-gen))
        p (clojure.test.check.properties/for-all* [gen-to-check] check-fn)]
    (clojure.test.check/quick-check iterations p
                                    :seed seed
                                    :max-size max-size
                                    :reporter-fn report-fn)))
