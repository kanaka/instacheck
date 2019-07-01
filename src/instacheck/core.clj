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
  "Takes an EBNF grammar, parser, file, or text string and returns
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

(defn- clj-prefix [nsname]
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
  (str (clj-prefix (:namespace ctx))
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

(defn parse-weights [parser texts]
  (let [grammar (grammar/parser->grammar parser)
        texts (if (string? texts) [texts] texts)
        parsed (map #(parse parser %) texts)]
    (apply merge-with +
           (map #(grammar/path-log-wtrek grammar %) parsed))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test execution

(defn quick-check
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

;; TODO: simpler ease function (defn check [check-fn ebnf & [opts]] ...)

