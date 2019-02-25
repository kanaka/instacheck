(defproject instacheck "0.1.0-SNAPSHOT"
  :description "Property-based testing with inputs defined as EBNF grammars"
  :url "https://github.com/kanaka/instacheck"
  :license {:name "Mozilla Public License version 2"
            :url "https://www.mozilla.org/en-US/MPL/2.0/"}

  :source-paths ["src"]

  :dependencies [[org.clojure/clojure "1.10.0"]
                 [com.rpl/specter "1.0.0"]
                 [com.gfredericks/test.chuck "0.2.9"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.clojure/test.check "0.10.0-alpha3"]

                 ;; Patched version
                 [instaparse "1.4.9.1-SNAPSHOT"]]

  :main instacheck.cli)
