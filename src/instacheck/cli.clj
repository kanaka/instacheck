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
;; Command line utilities

(defn sample-path
  [dir suffix]
  (str (io/file dir (if (number? suffix)
                      (format "sample-%04d" suffix)
                      (format "sample-%s" suffix)))))

(defn output-samples
  [ctx dir samples]
  (printf "Saving samples to %s - %s\n"
          (sample-path dir 0) (sample-path dir (count samples)))
  (io/make-parents (sample-path dir 0))
  (doseq [[idx sample] (map-indexed vector samples)]
    (let [f (io/file (sample-path dir idx))]
      (spit f sample))))

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

(defn save-weights [ctx file]
  (when file
    (pr-err "Saving weights to" file)
    (spit file (with-out-str (pprint (into (sorted-map)
                                           @(:weights-res ctx)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command line usage of ebnf

(def general-cli-options
  [[nil "--debug"
    "Add debug comments to generated code"]
   [nil "--verbose"
    "Verbose output during execution"]
   [nil "--weights WEIGHTS"
    "An EDN data file containing frequency weights"
    :default nil
    :parse-fn #(edn/read-string (slurp %))]
   [nil "--weights-output WEIGHTS-OUTPUT"
    "Write all resulting frequency weights out the file."]
   [nil "--start START"
    "Starting grammar rule"]
   ["-h" "--help"]])

(def cmd-options
  {"clj"     [[nil "--function FUNCTION"
               "Generate one function that returns a map of generators"]]
   "samples" [[nil "--samples SAMPLES"
               "Number of samples to generate"
               :default 10
               :parse-fn #(Integer. %)]]
   "check"   [["-s" "--seed SEED"
               "Random seed to use (otherwise automatic)"
               :parse-fn #(Integer. %)]
              [nil "--iterations ITERATIONS"
               "Check/test iterations"
               :default 10
               :parse-fn #(Integer. %)]
              [nil "--remove-samples"
               "Remove sample files after test"]]})

;; Gather up the general and command summary information
(def cli-summary
  (str "General Options:\n"
       (:summary (parse-opts [] general-cli-options))
       "\n"
       (string/join \newline
                    (for [[c co] cmd-options]
                      (str "\n" c " Options:\n"
                           (:summary (parse-opts [] co)))))))

(defn usage [& [errors]]
  (when (not (empty? errors))
    (pr-err (string/join \newline errors) "\n"))
  (pr-err "Usage:")
  (pr-err "  instacheck clj     [OPTIONS] <EBNF-FILE> <NAMESPACE>")
  (pr-err "  instacheck samples [OPTIONS] <EBNF-FILE> <SAMPLE_DIR>")
  (pr-err "  instacheck parse   [OPTIONS] <EBNF-FILE> <FILE>...")
  (pr-err "  instacheck check   [OPTIONS] <EBNF-FILE> <SAMPLE_DIR> -- <CMD>")
  (pr-err)
  (pr-err cli-summary)
  (System/exit 2))

(defn do-clj
  [ctx parser clj-ns function]
  (when (not clj-ns)
    (usage ["clj mode requires namespace"]))
  (let [grammar (instacheck/parser->grammar parser)
        gen-src (if function
                  instacheck/grammar->generator-func-source
                  instacheck/grammar->generator-defs-source)]

    (println
      (str (instacheck/clj-prefix clj-ns)
           (gen-src ctx grammar)))))

(defn do-samples
  [ctx parser dir number]
  (when (not dir)
    (usage ["samples mode requires SAMPLE_DIR"]))
  (let [grammar (instacheck/parser->grammar parser)
        samples (gen/sample (instacheck/ebnf-gen ctx grammar) number)]
    (output-samples ctx dir samples)))

(defn do-parse
  [ctx parser files]
  (when (empty? files)
    (usage ["parse mode requires FILE list"]))
  (let [weights
        (try
          (instacheck/parse-weights-from-files parser files)
          (catch Exception e
            (let [{:keys [text failure location]} (ex-data e)]
              (println (str "Parse error in '" location "':"))
              (println failure)
              (System/exit 1))))]
    ;; Update the ctx result weights
    (reset! (:weights-res ctx) weights)))

(defn do-check
  [ctx parser dir cmd opts]
  (when (not dir)
    (usage ["check mode requires SAMPLE_DIR"]))
  (when (empty? cmd)
    (usage ["check mode requires CMD args"]))
  (io/make-parents (sample-path dir 0))
  (let [grammar (instacheck/parser->grammar parser)
        cur-state (atom nil)
        cur-idx (atom 0)
        check-fn (fn [sample]
                   (run-test ctx
                             cmd
                             (sample-path dir (swap! cur-idx inc))
                             sample))
        report-fn (fn [r]
                    (when (:verbose ctx)
                      (prn :report (update-in
                                     (dissoc r :property)
                                     [:current-smallest] dissoc :function)))
                    (when (not (= @cur-state (:type r)))
                      (reset! cur-state (:type r))
                      (pr-err (str "NEW STATE: " (name (:type r))))))
        qc-res (instacheck/run-check
                 opts
                 (instacheck/ebnf-gen ctx grammar)
                 check-fn report-fn)]
    (println "Final Result:")
    (pprint qc-res)
    (when (not (:result qc-res))
      (let [fpath (sample-path dir "final")]
        (spit fpath (get-in qc-res [:shrunk :smallest 0]))
        (println "Smallest Failure:" fpath)))
    (:result qc-res)))

(defn -main
  [& args]
  (let [[[cmd & raw-args] [_ & check-cmd]] (split-with #(not= % "--") args)
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

        ;;_ (prn :ctx ctx)

        res (condp = cmd
              "clj"
              (do-clj ctx ebnf-parser (first cmd-args) (:function opts))
              "samples"
              (do-samples ctx ebnf-parser (first cmd-args) (:samples opts))
              "parse"
              (do-parse ctx ebnf-parser cmd-args)
              "check"
              (do-check ctx ebnf-parser (first cmd-args) check-cmd
                        (select-keys opts [:iterations :seed])))]

    (save-weights ctx (:weights-output opts))

    (if (= false res)
      (System/exit 1)
      (System/exit 0))))

