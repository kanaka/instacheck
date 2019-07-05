(ns instacheck.core
  (:require [clojure.string :as string]
            [clojure.test.check]
            [clojure.test.check.properties]
            [clojure.test.check.generators :as gen]
            [instaparse.core :as instaparse]

            [instacheck.grammar :as grammar]
            [instacheck.codegen :as codegen]

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
(def wtrek             grammar/wtrek)
(def path-log-wtrek    grammar/path-log-wtrek)
(def save-weights      grammar/save-weights)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better instaparse errors

(defn- elide-line
  [text cursor max-length cont-text]
  (let [span (/ max-length 2)
        start (max 0            (- cursor span))
        end   (min (count text) (+ cursor span))]
    (str
      (when (> cursor span) cont-text)
      (subs text start end)
      (when (> (count text) max-length) cont-text))))

(defn- concise-fail-str
  [failure text]
  (let [err-str (with-out-str (instaparse.failure/pprint-failure failure))
        column (:column failure)
        [pos text-line mark-line & reasons] (string/split err-str #"\n")
        text-line (string/replace text-line #"\t" " ")
        text-line (elide-line text-line column 200 "...")
        mark-line (elide-line mark-line column 200 "   ")

        reasons (string/join "\n" reasons)]
    (string/join "\n" [pos text-line mark-line reasons])))

(defn parse
  "Use parser to parse text. On success returns the parsed AST. On
  error, throws an ex-info object with a friendly error location
  string and a info map containing the :failure (instaparse Failure
  object), :text (the original text), and :location (optional location
  parameter)."
  [parser text & [location]]
  (let [res (parser text)]
    (if (instance? instaparse.gll.Failure res)
      ;;(throw (Exception. (concise-fail-str res text)))
      (throw (ex-info (concise-fail-str res text)
                      {:failure res
                       :text text
                       :location location}))
      res)))

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
        wtrek (grammar/path-log-wtrek grammar parsed)]
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
       :full  {PATH WEIGHT ...}}

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
  definition."
  [ebnf & [opts]]
  (let [{:keys [weights max-size]} opts
        gen (ebnf->gen {:weights weights} ebnf)]
    (if max-size
      (gen/sample-seq gen max-size)
      (gen/sample-seq gen))))

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

  The optional qc-opts parameter can be used to specify the following
  quick-check options:
    - iterations: number of iterations to check before success (default: 10)
    - max-size:   maximum size for all internal generators (default: 200)
    - seed:       starting seed for generating test cases
    - report-fn:  function to call with a report after each iteration"
  [check-fn ebnf-or-gen & [qc-opts]]
  (let [gen-to-check (if (= (type ebnf-or-gen)
                            clojure.test.check.generators.Generator)
                       ebnf-or-gen
                       (ebnf->gen {} ebnf-or-gen))
        {:keys [iterations seed max-size report-fn]
         :or {iterations 10
              ;;seed 1
              max-size 200
              report-fn (fn [& args] true)
              }} qc-opts
        p (clojure.test.check.properties/for-all* [gen-to-check] check-fn)]
    (clojure.test.check/quick-check iterations p
                                    :seed seed
                                    :max-size max-size
                                    :reporter-fn report-fn)))
