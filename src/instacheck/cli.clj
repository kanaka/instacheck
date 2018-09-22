(ns instacheck.cli
  (:require [clojure.string :as string]
            [clojure.edn :as edn]

            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.pprint :refer [pprint]]
            [clojure.test.check.generators :as gen]
            [clojure.tools.cli :refer [parse-opts]]

            [instaparse.core :as instaparse]

            [instacheck.core :as instacheck]

            ;; Used in the generated code. Useful to have loaded here
            ;; for REPL debugging.
            [com.gfredericks.test.chuck.generators :as chuck]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pr-err
  [& args]
  (binding [*out* *err*]
    (apply println args)
    (flush)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command line usage

(defn interpolate-path [template value]
  (let [subst (re-find #"%" template)
        template (if subst template (str template "%"))]
    (string/replace template #"%" (str value))))

(defn output-samples
  [ctx path-template samples]
  (doseq [[idx sample] (map-indexed vector samples)]
    (let [path (interpolate-path path-template idx)]
      (println "Generating" path)
      (spit path sample))))

(defn run-test
  [ctx raw-cmd sample-path sample]
  (let [sfile (clojure.java.io/as-file sample-path)
        swriter (io/writer sfile)
        cmd (if (seq (keep #(re-find #"%" %) raw-cmd))
              (map #(string/replace % #"%" sample-path) raw-cmd)
              (conj raw-cmd sample-path))
        res (try
              (println "Running:" (string/join " " cmd))
              (.write swriter sample)
              (.flush swriter)
              (apply sh cmd)
              (finally
                (if (:remove-samples ctx)
                  (.delete sfile))))]
    (when (:verbose ctx)
      (when (:out res) (print "Out:" (:out res)))
      (when (:err res) (print "Err:" (:err res))))
    (println "Result:"
             (if (= 0 (:exit res))
               "Pass"
               (str "Fail (exit code " (:exit res) ")")))
    (zero? (:exit res))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command line usage of ebnf

(def general-cli-options
  [[nil "--debug" "Add debug comments to generated code"]
   [nil "--verbose" "Verbose output during execution"]
   [nil "--weights WEIGHTS"
    "An EDN data file containing frequency weights (map of ctx path to weight value) to use (default weight is 100)."
    :default nil
    :parse-fn #(edn/read-string (slurp %))]
   [nil "--weights-output WEIGHTS-OUTPUT" "Write all resulting frequency weights out the file."]
   [nil "--start START" "Starting grammar rule"]])

(def cmd-options
  {"clj" [[nil "--namespace NAMESPACE" "Name of namespace to generate"]
          [nil "--function FUNCTION" "Generate a function (that returns a map of generators) rather then top-level generators"]]
   "samples" [[nil "--samples SAMPLES" "Number of samples to generate"
               :default 10]]
   "check" [[nil "--iterations ITERATIONS" "Check/test iterations"
             :default 10]
            [nil "--remove-samples" "Remove sample files after test"]]})

(defn opt-errors [opts]
  (when (:errors opts)
    (doall (map pr-err (:errors opts)))
    (System/exit 2))
  opts)

(defn usage []
  (pr-err "ebnf [GLOBAL-OPTS] clj     <EBNF-FILE> [CLJ-OPTS]")
  (pr-err "ebnf [GLOBAL-OPTS] samples <EBNF-FILE> [GEN-OPTS] <SAMPLE_TEMPLATE>")
  (pr-err "ebnf [GLOBAL-OPTS] check   <EBNF-FILE> [CHECK-OPTS] <SAMPLE_TEMPLATE> -- <CMD>")
  (pr-err "ebnf [GLOBAL-OPTS] parse   <EBNF-FILE> <FILE> [<FILE>...]")
  (System/exit 2))

(defn save-weights [ctx file]
  (when file
    (spit file (with-out-str (pprint (into (sorted-map)
                                           @(:weights-res ctx)))))))

(defn -main
  [& args]
  (let [top-opts (opt-errors (parse-opts args
                                         general-cli-options :in-order true))
        [cmd ebnf & cmd-args] (:arguments top-opts)
        _ (when (not (and ebnf
                          cmd
                          (#{"clj" "samples" "check" "parse"} cmd)))
            (usage))
        cmd-opts (opt-errors (parse-opts cmd-args
                                         (concat general-cli-options
                                                 (cmd-options cmd))
                                         :in-order true))
        opts (merge (:options top-opts)
                    (into {} (filter (comp not nil? second)
                                     (:options cmd-opts))))
        ctx (merge (select-keys opts [:debug :verbose :start
                                      :namespace :function :weights
                                      :grammar-updates])
                   {:weights-res (atom {})})
        ebnf-parser (instaparse/parser (slurp ebnf))
        ebnf-grammar (instacheck/trim-parser ebnf-parser)

        ;;_ (prn :ctx ctx)

        ;; Some additional sanity checks not captured by the CLI parser
        _ (when (and (= "clj" cmd)
                     (not (:namespace ctx)))
            (pr-err "--namespace NAMESPACE required")
            (System/exit 2))

        _ (when (and (= "samples" cmd)
                     (not (= 1 (count (:arguments cmd-opts)))))
            (usage))

        _ (prn :arguments (:arguments cmd-opts))
        _ (when (and (= "check" cmd)
                     (or
                       (< (count (:arguments cmd-opts)) 3)
                       (not (= "--" (nth (:arguments cmd-opts) 1)))))
            (usage))

        res (condp = cmd
              "clj"
              (let [gen-src (if (:function opts)
                              instacheck/grammar->generator-func-source
                              instacheck/grammar->generator-defs-source)]

                (println
                  (str (instacheck/clj-prefix (:namespace ctx))
                       (gen-src ctx ebnf-grammar))))

              "samples"
              (let [sample-template (first (:arguments cmd-opts))
                    samples (gen/sample (instacheck/ebnf-gen ctx ebnf-grammar)
                                        (Integer. (:samples opts)))]
                (output-samples ctx sample-template samples))

              "check"
              (let [cur-state (atom nil)
                    cur-idx (atom 0)
                    [sample-template _ & cmd] (:arguments cmd-opts)
                    qc-res (instacheck/run-check
                             (select-keys ctx [:iterations])
                             (instacheck/ebnf-gen ctx ebnf-grammar)
                             (fn [sample]
                               (run-test ctx
                                         cmd
                                         (interpolate-path sample-template
                                                           (swap! cur-idx inc))
                                         sample))
                             (fn [r]
                               (when (:verbose ctx)
                                 (prn :report (update-in
                                                (dissoc r :property)
                                                [:current-smallest] dissoc :function)))
                               (when (not (= @cur-state (:type r)))
                                 (reset! cur-state (:type r))
                                 (println (str "NEW STATE: " (name (:type r)))))))]
                (println "Final Result:")
                (pprint qc-res)
                (let [fpath (interpolate-path sample-template "final")]
                  (spit fpath (get-in qc-res [:shrunk :smallest 0]))
                  (println "Smallest Failure:" fpath))
                (:result qc-res))

              "parse"
              (let [;; Get the full set of zero'd out weights by
                    ;; calling the def generator but throwing away the
                    ;; result. The weights are in the context atom.
                    _ (instacheck/grammar->generator-defs-source ctx ebnf-grammar)
                    base-weights (into {} (for [[k v] @(:weights-res ctx)]
                                            [k 0]))
                    ;; Parse each file to get a cumulative list of
                    ;; grammar paths
                    paths (mapcat #(-> (instaparse/parse ebnf-parser (slurp %)
                                                    :unhide :all)
                                       meta
                                       :path-log)
                                  (:arguments cmd-opts))]
                ;; Merge the grammar path frequencies onto the zero'd
                ;; out weights
                (reset! (:weights-res ctx)
                        (merge base-weights
                               (frequencies paths)))))]

    (when-let [wfile (:weights-output opts)]
      (pr-err "Saving weights to" wfile)
      (save-weights ctx wfile))

    (if (= false res)
      (System/exit 1)
      (System/exit 0))))

