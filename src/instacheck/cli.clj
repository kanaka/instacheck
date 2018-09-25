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
    "An EDN data file containing frequency weights"
    :default nil
    :parse-fn #(edn/read-string (slurp %))]
   [nil "--weights-output WEIGHTS-OUTPUT" "Write all resulting frequency weights out the file."]
   [nil "--start START" "Starting grammar rule"]
   ["-h" "--help"]])

(def cmd-options
  {"clj" [[nil "--function FUNCTION" "Generate one function that returns a map of generators"]]
   "samples" [[nil "--samples SAMPLES" "Number of samples to generate"
               :default 10
               :parse-fn #(Integer. %)]]
   "check" [[nil "--iterations ITERATIONS" "Check/test iterations"
             :default 10
             :parse-fn #(Integer. %)]
            [nil "--remove-samples" "Remove sample files after test"]]})

;;(defn opt-errors [opts]
;;  (when (:errors opts)
;;    (doall (map pr-err (:errors opts)))
;;    (System/exit 2))
;;  opts)

(defn usage [summary & [errors]]
  (when (not (empty? errors))
    (pr-err (string/join \newline errors) "\n"))
  (pr-err "Usage:")
  (pr-err "  instacheck clj     [OPTIONS] <EBNF-FILE> <NAMESPACE>")
  (pr-err "  instacheck samples [OPTIONS] <EBNF-FILE> <SAMPLE_TEMPLATE>")
  (pr-err "  instacheck check   [OPTIONS] <EBNF-FILE> <SAMPLE_TEMPLATE> -- <CMD>")
  (pr-err "  instacheck parse   [OPTIONS] <EBNF-FILE> <FILE>...")
  (pr-err)
  (pr-err summary)
  (System/exit 2))

(defn save-weights [ctx file]
  (when file
    (spit file (with-out-str (pprint (into (sorted-map)
                                           @(:weights-res ctx)))))))

(defn -main
  [& args]
  (let [[[cmd & raw-args] [_ & check-cmd]] (split-with #(not= % "--") args)
        ;; Gather up the general and command summary information
        summary (str "General Options:\n"
                     (:summary (parse-opts [] general-cli-options))
                     "\n"
                     (string/join \newline
                                  (for [[c co] cmd-options]
                                    (str "\n" c " Options:\n"
                                         (:summary (parse-opts [] co))))))
        usage (partial usage summary)

        ;; Now do the command specific parsing
        _ (when-not (#{"clj" "samples" "check" "parse"} cmd)
            (usage [(str "Unknown command " cmd)]))
        cmd-opts (parse-opts raw-args (concat general-cli-options
                                              (cmd-options cmd)))
        _ (when (:help (:options cmd-opts)) (usage))
        _ (when (:errors cmd-opts) (usage (:errors cmd-opts)))
        [ebnf & cmd-args] (:arguments cmd-opts)
        _ (when (not ebnf) (usage ["EBNF-FILE required"]))
        opts (into {} (filter (comp not nil? second)
                              (:options cmd-opts)))
        ctx (merge (select-keys opts [:debug :verbose :start
                                      :namespace :function :weights
                                      :grammar-updates])
                   {:weights-res (atom {})})
        ebnf-parser (instaparse/parser (slurp ebnf))
        ebnf-grammar (instacheck/trim-parser ebnf-parser)

        ;;_ (prn :ctx ctx)

        ;; Some additional sanity checks not captured by the CLI parser
        _ (when (and (= "clj" cmd) (not (= 1 (count cmd-args))))
            (usage ["Too few arguments for clj command"]))

        _ (when (and (= "samples" cmd) (not (= 1 (count cmd-args))))
            (usage ["Too few arguments for samples command"]))

        _ (when (and (= "check" cmd) (or (< (count cmd-args) 1)
                                         (empty? check-cmd)))
            (usage ["Too few arguments for check command"]))

        res (condp = cmd
              "clj"
              (let [clj-ns (first cmd-args)
                    gen-src (if (:function opts)
                              instacheck/grammar->generator-func-source
                              instacheck/grammar->generator-defs-source)]

                (println
                  (str (instacheck/clj-prefix clj-ns)
                       (gen-src ctx ebnf-grammar))))

              "samples"
              (let [sample-template (first cmd-args)
                    samples (gen/sample (instacheck/ebnf-gen ctx ebnf-grammar)
                                        (:samples opts))]
                (output-samples ctx sample-template samples))

              "check"
              (let [sample-template (first cmd-args)
                    cur-state (atom nil)
                    cur-idx (atom 0)
                    qc-res (instacheck/run-check
                             (select-keys opts [:iterations])
                             (instacheck/ebnf-gen ctx ebnf-grammar)
                             (fn [sample]
                               (run-test ctx
                                         check-cmd
                                         (interpolate-path
                                           sample-template (swap!  cur-idx inc))
                                         sample))
                             (fn [r]
                               (when (:verbose ctx)
                                 (prn :report (update-in
                                                (dissoc r :property)
                                                [:current-smallest] dissoc :function)))
                               (when (not (= @cur-state (:type r)))
                                 (reset! cur-state (:type r))
                                 (pr-err (str "NEW STATE: " (name (:type r)))))))]
                (println "Final Result:")
                (pprint qc-res)
                (when (not (:result qc-res))
                  (let [fpath (interpolate-path sample-template "final")]
                    (spit fpath (get-in qc-res [:shrunk :smallest 0]))
                    (println "Smallest Failure:" fpath)))
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
                                  cmd-args)]
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

