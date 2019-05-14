(defproject kanaka/instacheck "0.6.2"
  :description "Property-based testing with inputs defined as EBNF grammars"
  :url "https://github.com/kanaka/instacheck"
  :license {:name "Mozilla Public License version 2"
            :url "https://www.mozilla.org/en-US/MPL/2.0/"}

  :source-paths ["src"]

  :dependencies [[org.clojure/clojure "1.10.0"]
                 [com.rpl/specter "1.0.0"]
                 [com.gfredericks/test.chuck "0.2.9"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.clojure/test.check "0.10.0-alpha4"]

                 ;; Patched version (retain comments, parse path log)
                 [kanaka/instaparse "1.4.9.1"]]

  :main instacheck.cli)
