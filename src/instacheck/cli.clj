(ns instacheck.cli
  (:require [clojure.string :as string]
            [clojure.edn :as edn]

            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.pprint :refer [pprint]]
            [clojure.test.check.generators :as gen]
            [clojure.tools.cli :refer [parse-opts]]

            [instaparse.core :as instaparse]

            [instacheck.core :as icore]
            [instacheck.grammar :as igrammar]
            [instacheck.weights :as iweights]
            [instacheck.reduce :as ireduce]))

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
  {"clj"        [[nil "--function FUNCTION"
                  "Generate one function named FUNCTION that returns a map of generators"]]
   "samples"    [[nil "--samples SAMPLES"
                  "Number of samples to generate"
                  :default 10
                  :parse-fn #(Integer. %)]]
   "check-once" [["-s" "--seed SEED"
                  "Random seed to use (otherwise automatic)"
                  :parse-fn #(Integer. %)]
                 [nil "--iterations ITERATIONS"
                  "Check/test iterations (size increases for one full test run)"
                  :default 10
                  :parse-fn #(Integer. %)]]
   "check"      [[nil "--iterations ITERATIONS"
                  "Check/test iterations (size increases for one full test run)"
                  :default 10
                  :parse-fn #(Integer. %)]
                 [nil "--runs RUNS"
                  "Number of times to run test iterations (total tests = runs X iterations)."
                  :default 1
                  :parse-fn #(Integer. %)]
                 [nil "--reduce-fails"
                  "After each run that finds a failure, reduce the weight of the "
                  :default 1
                  :parse-fn #(Integer. %)]]
   "parse"       [[nil "--ebnf-output EBNF-OUTPUT"
                   "Write subset of original EBNF to file that represents the parsed text."]]})

;; Gather up the general and command summary information
(def cli-summary
  (str "General Options:\n"
       (:summary (parse-opts [] general-cli-options))
       "\n"
       (string/join \newline
                    (for [[c co] cmd-options]
                      (str "\n" c " Options:\n"
                           (:summary (parse-opts [] co)))))))

(defn pr-err
  [& args]
  (binding [*out* *err*]
    (apply println args)
    (flush)))

(defn usage [& [errors]]
  (when (not (empty? errors))
    (pr-err (string/join \newline errors) "\n"))
  (pr-err "Usage:")
  (pr-err "  instacheck clj        [OPTIONS] <EBNF-FILE> <NAMESPACE>")
  (pr-err "  instacheck samples    [OPTIONS] <EBNF-FILE> <SAMPLE-DIR>")
  (pr-err "  instacheck parse      [OPTIONS] <EBNF-FILE> <FILE>...")
  (pr-err "  instacheck check-once [OPTIONS] <EBNF-FILE> <SAMPLE-DIR> -- <CMD>")
  (pr-err "  instacheck check      [OPTIONS] <EBNF-FILE> <SAMPLE-DIR> -- <CMD>")
  (pr-err)
  (pr-err cli-summary)
  (System/exit 2))


;; Command line utilities

(defn sample-path
  [dir suffix]
  (str (io/file dir (if (number? suffix)
                      (format "sample-%04d" suffix)
                      (format "sample-%s" suffix)))))

;; do-clj

(defn do-clj
  [ctx parser clj-ns]
  (let [grammar (igrammar/parser->grammar parser)]
    (println (icore/grammar->ns (assoc ctx :namespace clj-ns) grammar))))

;; do-samples

(defn output-samples
  [ctx dir samples]
  (printf "Saving samples to %s - %s\n"
          (sample-path dir 0) (sample-path dir (count samples)))
  (io/make-parents (sample-path dir 0))
  (doseq [[idx sample] (map-indexed vector samples)]
    (let [f (io/file (sample-path dir idx))]
      (spit f sample))))

(defn do-samples
  [ctx parser dir number]
  (let [genfn (icore/ebnf->gen ctx parser)
        samples (gen/sample genfn number)]
    (output-samples ctx dir samples)))

;; do-parse

(defn do-parse
  [ctx parser files]
  (let [texts-ids (map #(vector (slurp %) %) files)
        data (try
               (icore/parse-wtreks parser texts-ids)
               (catch Exception e
                 (let [{:keys [text failure location]} (ex-data e)]
                   (println (str "Parse error in '" location "':"))
                   (println failure)
                   (System/exit 1))))
        weights (:full-wtrek data)]
    (when (:ebnf-output ctx)
      (let [grammar (igrammar/parser->grammar parser)
            pruned-grammar (ireduce/prune-grammar
                             grammar {:wtrek weights})
            order (into {} (filter #(= 1 (count (key %)))
                                   (iweights/distance-trek grammar)))
            pruned-ebnf (igrammar/grammar->ebnf
                          (sort-by #(get order [(key %)])
                                   pruned-grammar))]
        (spit (:ebnf-output ctx) (str pruned-ebnf "\n"))))
    ;; Update the ctx result weights
    (reset! (:weights-res ctx) weights)
    (iweights/print-weights weights)))

;; do-check

(defn run-cmd
  [ctx raw-cmd sample-path]
  (let [cmd (if (seq (keep #(re-find #"%" %) raw-cmd))
              (map #(string/replace % #"%" sample-path) raw-cmd)
              (conj raw-cmd sample-path))
        _ (println "Running:" (string/join " " cmd))
        res (apply sh cmd)]
    (when (:verbose ctx)
      (when (:out res) (print "Out:" (:out res)))
      (when (:err res) (print "Err:" (:err res))))
    (println "Result:"
             (if (= 0 (:exit res))
               "Pass"
               (str "Fail (exit code " (:exit res) ")")))
    (zero? (:exit res))))

(defn run-test
  [ctx check-fn sample-path sample]
  (let [sfile (clojure.java.io/as-file sample-path)
        swriter (io/writer sfile)]
    (.write swriter sample)
    (.flush swriter)
    (check-fn ctx sample-path)))

(defn check-and-report
  [ctx generator dir check-fn opts]
  (io/make-parents (sample-path dir 0))
  (let [cur-state (atom nil)
        cur-idx (atom 0)
        wrap-check-fn (fn [sample]
                        (run-test ctx
                                  check-fn
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
        qc-opts (merge opts {:report-fn report-fn})
        res (icore/instacheck wrap-check-fn generator qc-opts)]
    res))

(defn do-check
  [ctx parser dir check-fn opts]
  (io/make-parents (sample-path dir 0))
  (loop [run 1
         qc-res {:result true}]
    (if (> run (:runs opts))
      qc-res
      (let [;; For more than 1 run, add a run subdirectory
            run-dir (if (> (:runs opts) 1)
                      (str (io/file dir (format "%04d" run)))
                      dir)
            res-file (io/file run-dir "result.edn")
            weights-file (io/file run-dir "weights.edn")
            generator (icore/ebnf->gen ctx parser)
            qc-res (check-and-report ctx generator run-dir check-fn opts)]
        (pr-err "Saving weights to" (str weights-file))
        (iweights/save-weights weights-file @(:weights-res ctx))
        (pr-err "Saving result map to" (str res-file))
        (spit res-file qc-res)
        (println "Result:")
        (pprint qc-res)
        (when (not (:result qc-res))
          (let [fpath (sample-path run-dir "final")]
            (spit fpath (get-in qc-res [:shrunk :smallest 0]))
            (println "Smallest Failure:" fpath)))
        (recur (inc run) qc-res)))))

(defn -main
  [& args]
  (let [;; General parsing
        [[cmd & raw-args] [_ & check-cmd]] (split-with #(not= % "--") args)
        ;; Command specific parsing
        _ (when-not (#{"clj" "samples" "check-once" "check" "parse"} cmd)
            (usage [(str "Unknown command " cmd)]))
        cmd-opts (parse-opts raw-args (concat general-cli-options
                                              (cmd-options cmd)))
        _ (when (:help (:options cmd-opts)) (usage))
        _ (when (:errors cmd-opts) (usage (:errors cmd-opts)))
        check-fn (when check-cmd
                   (fn [ctx sample-path]
                     (run-cmd ctx check-cmd sample-path)))
        [ebnf & cmd-args] (:arguments cmd-opts)
        _ (when (not ebnf) (usage ["EBNF-FILE required"]))
        opts (into {} (filter (comp not nil? second)
                              (:options cmd-opts)))
        _ (when (:verbose opts) (pr-err "Loading parser from" ebnf))
        ebnf-parser (instaparse/parser (slurp ebnf))
        _ (when (:verbose opts) (pr-err "Extracting comment weights"))
        comment-weights (igrammar/comment-trek
                          (igrammar/parser->grammar ebnf-parser)
                          :weight)
        ctx (merge (select-keys opts [:debug :verbose :start
                                      :namespace :function
                                      :grammar-updates :ebnf-output])
                   {:weights (merge comment-weights
                                    (:weights opts))
                    :weights-res (atom {})}
                   (when (:verbose opts)
                     {:log-fn pr-err}))

        ;;_ (prn :ctx ctx)

        res (condp = cmd
              "clj"
              (let [clj-ns (first cmd-args)]
                (when (not clj-ns) (usage ["clj mode requires namespace"]))
                (do-clj ctx ebnf-parser clj-ns))
              "samples"
              (let [dir (first cmd-args)]
                (when (not dir) (usage ["samples mode requires SAMPLE_DIR"]))
                (do-samples ctx ebnf-parser dir (:samples opts)))
              "parse"
              (let [files cmd-args]
                (when (empty? files) (usage ["parse mode requires FILE list"]))
                (do-parse ctx ebnf-parser files))
              "check-once"
              (let [dir (first cmd-args)]
                (when (not dir) (usage ["check mode requires SAMPLE_DIR"]))
                (when (not check-fn) (usage ["check mode requires CMD args"]))
                (do-check ctx ebnf-parser dir check-fn
                          (merge (select-keys opts [:seed :iterations])
                                 {:runs 1})))
              "check"
              (let [dir (first cmd-args)]
                (when (not dir) (usage ["check mode requires SAMPLE_DIR"]))
                (when (not check-fn) (usage ["check mode requires CMD args"]))
                (do-check ctx ebnf-parser dir check-fn
                          (select-keys opts [:runs :iterations]))))]

    (when-let [weights-output (:weights-output opts)]
      (iweights/save-weights weights-output @(:weights-res ctx)))

    (if (= false res)
      (System/exit 1)
      (System/exit 0))))

