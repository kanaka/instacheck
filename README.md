# Instacheck 0.9.3

*Instaparse meets test.check: property-based testing with inputs defined as EBNF grammars*

If the test inputs for your program are defined by `input.ebnf` then
you can use property-base testing to test your program like this:

```
lein run check input.ebnf sample-dir/ -- ./prog %
```

This will run `./prog` with larger and larger sample files (stored in
`sample-dir`) until the program fails. Then it will run `./prog` with
smaller and smaller versions of the failure case until it finds the
smallest version that still fails.

## Library / REPL QuickStart

Add the following to your Clojure dependencies:

```clojure
[kanaka/instacheck "0.9.3"]
```

Here is a simple example of using instacheck to test a function:

```clojure
(require '[instacheck.core :refer [instacheck]])
(def ebnf "root = ('foo' #'[0-9]' ) 'bar' *")
(defn check-func [test-case] (<= (count test-case) 7))

;; a failure will be detected and shrunk
(instacheck check-func ebnf)

;; a report will be printed for each iteration
(instacheck check-func ebnf {:report-fn #(prn %)})

;; manual seed for repeatable results 
(instacheck check-func ebnf {:seed 2})
```

In practice your check-fn will be a wrapper function that calls the
the function you are testing and then verifies it's behavior. One
common case is where you have some kind of "test oracle" that verifies
the behavior/output of the function you are testing:

```clojure
(defn check-func [test-case]
  (let [res (func-to-test test-case)]
    (oracle-func test-case res)))
```

Another common case is where a test failure manifests as an exception:

```clojure
(defn check-func [test-case]
  (try
    (func-to-test test-case)
    true
    (catch Throwable t
      false)))
```


Here is an example of using instacheck with instaparse and test.check:

```clojure
(require '[instacheck.core :as instacheck])
(require '[instaparse.core :as instaparse])
(require '[clojure.test.check :as tc])
(require '[clojure.test.check.generators :as tc-gen])
(require '[clojure.test.check.properties :as tc-prop])

;; parser is a regular instaparse parser
(def parser (instaparse/parser "root = ('foo' #'[0-9]' ) 'bar' *"))

;; Generate some samples using ebnf-sample-seq from instacheck
(take 20 (instacheck/ebnf-sample-seq parser))

;; gen is a regular test.check generator based on the parser
(def gen (instacheck/ebnf->gen {} parser))

;; Generate some samples using sample-gen from test.check
(tc-gen/sample gen)

;; A test.check input property with gen
(def prop (tc-prop/for-all* [gen] #(<= (count %) 7)))
;; Run test.check quick-check for 10 iterations on prop
(tc/quick-check 10 prop)
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
    # change the weight for "7" [:nz-digit :alt 6] to 1000
lein run samples test/bc.ebnf --weights tmp/bc-weights.edn tmp/
```

Generate test samples and run test program using the test samples.
When a failure is found then continue testing until a more minimal
test sample is found. Then manually update the weights file to
increase the likelihood of 0 numbers (and thus a failure due to divide
by zero) and then run the tests again:

```bash
rm tmp/samp*
lein run check test/bc.ebnf --weights tmp/bc-weights.edn tmp/ -- test/testbc.sh %
    # increase frequency of "0" [:any-number :alt 0] to 1000
rm tmp/samp*
lein run check test/bc.ebnf --weights tmp/bc-weights.edn tmp/ -- test/testbc.sh %
```

Parse weights out of an existing test case then use those weights to
generate similar samples:

```bash
lein run parse test/bc.ebnf test/bc.samp1 > tmp/bc-weights2.edn
rm tmp/samp*
lein run samples test/bc.ebnf --weights tmp/bc-weights2.edn tmp/
```


## License

Copyright © Joel Martin

Distributed under the Mozilla Public License either version 2.0 or (at
your option) any later version. See LICENSE for more details.
