(ns instacheck.grammar
  (:require [instacheck.util :as util]
            [instaparse.core :as instaparse]))

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

