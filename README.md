# Instacheck

**Instaparse meets test.check: property-based testing with inputs defined as EBNF grammars**

## Prerequisites:

* Build/install patched instaparse that retains comment data and does
  grammar traversal logging:
```bash
git clone git@github.com:kanaka/instaparse.git
cd instaparse
lein install
```

## Library Usage

Add the following to your Clojure dependencies:

```clojure
[instacheck "0.4.0"]
```

Require instacheck and use it to generate test cases based on a
grammar and check those test cases with a check-fn:

```clojure
(ns example.core
  (:require [instacheck.core :as instacheck]))

(let [grammar "root = ('foo' #'[0-9]' ) 'bar' *"
      grammar-obj (instacheck/load-grammar grammar)
      generator (instacheck/ebnf-gen {} grammar-obj)
      check-fn #(do (prn :sample %) (< (count %) 5))
      report-fn #(prn :report %)]
  (instacheck/run-check {:iterations 5} generator check-fn report-fn))
```


## Commandline Usage

All the following example use the `test/bc.ebnf` EBNF grammar file
which specifies a simple EBNF for generating commands that can be run
with the bc (arbitrary precission calculator) program.

Generate Clojure generators (one generator per EBNF rule named after
the non-terminal):

```bash
lein run clj test/bc.ebnf bc.test
```

Generate a single Clojure generator (one generator named `gen-gc`):

```bash
lein run clj test/bc.ebnf bc.test --function gen-bc
```

Generate 10 and then 100 samples:

```bash
lein run samples test/bc.ebnf tmp/
lein run samples test/bc.ebnf --samples 100 tmp/
```

Output the full set of weights to a file, modify the weights file and
then generate 10 samples using the modified weights file:

```bash
rm tmp/samp*
lein run samples test/bc.ebnf --weights-output tmp/bc-weights.edn tmp/
    # change the weight for 7 (:nz-digit :alt 6) to 1000
lein run samples test/bc.ebnf --weights tmp/bc-weights.edn tmp/
```

Generate test samples and run test program using the test samples.
When a failure is found then continue testing until a more minimal
test sample is found. Then manually update the weights file to
increase the likelihood of 0 numbers (and thus a failure due to divide
by zero) and then run the tests again:

```bash
rm tmp/samp*
lein run check test/bc.ebnf --weights tmp/bc-weights.edn tmp/ -- test/testbc.sh -q %
    # tweak 0 to increase frequency
lein run check test/bc.ebnf --weights tmp/bc-weights.edn tmp/ -- test/testbc.sh -q %
```

## License

Copyright © Joel Martin

Distributed under the Mozilla Public License either version 2.0 or (at
your option) any later version.
