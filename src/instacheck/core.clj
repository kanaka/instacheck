(ns instacheck.core
  (:require [clojure.string :as string]
            [instaparse.core :as instaparse]
            [clojure.test.check]
            [clojure.test.check.properties]
            [clojure.test.check.generators :as gen]

            [instacheck.util :as i-util]
            [instacheck.grammar :as i-grammar]
            [instacheck.codegen :as i-codegen]

            ;; Convenient to have already loaded for testing
            [clojure.pprint :refer [pprint]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generator object/API

(defn grammar->generator-obj
  "Return a a new generator object based on the context and instacheck
  grammar object."
  [{:keys [start] :as ctx} grammar]
  (let [ctx (assoc ctx :function "ephemeral")
        fn-src (i-codegen/grammar->generator-func-source ctx grammar)
        gen-fn (i-codegen/eval-generator-source fn-src)
        start (or start (:start (meta grammar)))
        gens (gen-fn (:weights ctx))
        gen (gen/fmap i-util/flatten-text (get gens start))]
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
        gen (gen/fmap i-util/flatten-text (get gens start))]
    (with-meta
      gen
      (assoc (meta obj)
             :cur-start start
             :cur-generator gen))))

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
       (i-grammar/load-grammar ebnf)      ;; Text string or file
       (if (:grammar ebnf)
         (i-grammar/parser->grammar ebnf) ;; Parser
         ebnf)))))                      ;; Assume grammar


(comment
  (println (grammar->generator-function-source {} (i-grammar/load-grammar (slurp "test/recur3.ebnf"))))

  (def ebnf-generator (ebnf->gen (slurp "test/recur3.ebnf")))
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
  (str (clj-prefix (:namespace ctx))
       (i-codegen/grammar->generator-defs-source ctx grammar)))

(comment
  (def ebnf-grammar (i-grammar/load-grammar (slurp "test/recur1.ebnf")))
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

;;;;;

;; Make some grammar definitions available from core
(def filter-alts i-grammar/filter-alts)
(def parse-grammar-comments i-grammar/parse-grammar-comments)

(defn parse-weights
  "Use parser to parse a sequence of text description objects {:text
  text :location location}. Returns a weights map with the weights set
  to the number of times that path in the grammar was followed/used
  across all the texts from text-objs."
  [parser text-objs]
  (let [grammar (i-grammar/parser->grammar parser)
	;; Get the full set of zero'd out weights by
	;; calling the def generator but throwing away the
	;; result. The weights are in the context atom.
        ctx {:weights-res (atom {})}
        _ (i-codegen/grammar->generator-defs-source ctx grammar)
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

