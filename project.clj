(defproject kanaka/instacheck "0.8.4"
  :description "Property-based testing with inputs defined as EBNF grammars"
  :url "https://github.com/kanaka/instacheck"
  :license {:name "Mozilla Public License version 2"
            :url "https://www.mozilla.org/en-US/MPL/2.0/"}

  :source-paths ["src"]

  :dependencies [[org.clojure/clojure "1.10.0"]
                 [com.gfredericks/test.chuck "0.2.9"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.clojure/test.check "0.10.0-alpha4"]

                 ;; Patched version (retain comments, parse path log)
                 [kanaka/instaparse "1.4.9.2"]

                 ;; ClojureScript
                 [org.clojure/clojurescript "1.10.520"]
                 [cljs-node-io "1.1.2"]]

  :profiles {:cli  {:main instacheck.cli}
             :core {:main instacheck.core}}

  :plugins [[lein-cljsbuild "1.1.7"]]

  :cljsbuild
  {:builds {:web
            {:source-paths ["src"]
             :compiler
             {:main          "instacheck.core"
              :asset-path    "/build/"
              :output-to     "build/instacheck.js"
              :output-dir    "build/web/"
              :source-map    true
              :optimizations :none
              :pretty-print  true}}

            ;; TODO: convert and use instacheck.cli for :main
            :node
            {:source-paths ["src"]
             :compiler
             {:target        :nodejs
              :main          "instacheck.core"
              :output-to     "instacheck.js"
              :output-dir    "build/node"
              :source-map    true
              :optimizations :none
              :pretty-print  true}} }}

  :main instacheck.cli)
