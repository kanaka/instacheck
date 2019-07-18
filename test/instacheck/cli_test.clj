(ns instacheck.cli-test
  (:require [clojure.test :refer [deftest testing is]]
            [instacheck.cli :as cli]))


(deftest sample-path-test
  (testing "sample-path"
    (is (= (cli/sample-path "dir1" "foo")
           "dir1/sample-foo"))
    (is (= (cli/sample-path "dir1" 9)
           "dir1/sample-0009"))))
