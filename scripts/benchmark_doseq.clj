;; lein bench-doseq{-quick}
(ns benchmark-doseq
  (:require [criterium.core :as c]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.core :as exponential]
            [io.github.frenchy64.fully-satisfies.linear-expansion :as linear]
            [cheshire.core :as json]
            [clojure.java.io :as io])
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

;; benchmark runs equivalent of this code, for each doseq
;; n == size
;; (fn []
;;   (doseq [x0 (range 10)
;;           ...
;;           xn (range 10)]
;;     (str x0 ... xn)))

(defn bench* [quick? size]
  (let [bench-fn (if quick? c/quick-benchmark* c/benchmark*)
        syms (mapv (fn [i] (symbol "x" i)) (range size))
        binder (vec (interpose syms (repeat `(range 10))))
        body (list* `str syms)
        op->form (fn [op] (list op binder body))
        form->runner (fn [form]
                       (eval (list `fn [] form)))
        opts {}]
    (into (sorted-map)
          (map (fn [op] {op (let [form (op->form op)
                                  runner (form->runner form)]
                              (assert (not (Thread/interrupted)))
                              (assoc (bench-fn runner opts)
                                     :form form))}))
          [`exponential/doseq
           `linear/doseq])))

(defn extract-graph-data
  "Extracts mean execution times from benchmark results for graphing.
   Returns a sequence of maps suitable for Vega-Lite."
  [results]
  (mapcat
   (fn [result-map]
     (let [size (first (keys result-map))
           implementations (get result-map size)]
       (map
        (fn [[impl-name impl-data]]
          (let [mean-time (first (:mean impl-data))
                impl-label (str impl-name)]
            {:size size
             :time (* mean-time 1e6) ; convert to microseconds for readability
             :implementation impl-label}))
        implementations)))
   results))

(defn export-vega-html!
  "Exports a Vega-Lite spec to an HTML file with embedded visualization.
   Takes a Vega-Lite spec (as a Clojure map) and an output file path."
  [spec output-file]
  (let [spec-json (json/generate-string spec)
        html (str "<!DOCTYPE html>\n"
                  "<html>\n"
                  "<head>\n"
                  "  <meta charset=\"UTF-8\">\n"
                  "  <title>Benchmark Visualization</title>\n"
                  "  <script src=\"https://cdn.jsdelivr.net/npm/vega@5\"></script>\n"
                  "  <script src=\"https://cdn.jsdelivr.net/npm/vega-lite@6.4.1\"></script>\n"
                  "  <script src=\"https://cdn.jsdelivr.net/npm/vega-embed@6\"></script>\n"
                  "  <style>\n"
                  "    body { padding: 20px; font-family: sans-serif; }\n"
                  "  </style>\n"
                  "</head>\n"
                  "<body>\n"
                  "  <div id=\"vis\"></div>\n"
                  "  <script>\n"
                  "    vegaEmbed('#vis', " spec-json ", {\"mode\": \"vega-lite\"});\n"
                  "  </script>\n"
                  "</body>\n"
                  "</html>\n")]
    (spit output-file html)))

(defn render-graph
  "Renders a 2D line graph comparing doseq implementations using Vega-Lite."
  [graph-data output-file]
  (let [spec {:data {:values graph-data}
              :title "doseq Performance Comparison"
              :width 600
              :height 400
              :mark {:type "line" :point true}
              :encoding {:x {:field "size"
                            :type "quantitative"
                            :title "Number of bindings"}
                        :y {:field "time"
                            :type "quantitative"
                            :title "Mean execution time (μs)"
                            :scale {:type "log"}}
                        :color {:field "implementation"
                               :type "nominal"
                               :title "Implementation"}
                        :tooltip [{:field "size" :type "quantitative"}
                                  {:field "time" :type "quantitative" :format ".4f"}
                                  {:field "implementation" :type "nominal"}]}}]
    (export-vega-html! spec output-file)
    (println (str "Graph saved to: " output-file))))

(defn bench
  ([] (bench false))
  ([quick?] (bench quick? 0))
  ([quick? iterations]
   (let [results (mapv #(bench* quick? %) (range iterations))
         timestamp (.format (LocalDateTime/now) (DateTimeFormatter/ofPattern "yyyyMMdd-HHmmss"))
         output-dir "bench-results"
         results-file (str output-dir "/benchmark-results-" timestamp ".edn")
         graph-file (str output-dir "/benchmark-graph-" timestamp ".html")]
     ;; Create output directory if it doesn't exist
     (io/make-parents results-file)

     ;; Save raw results to file
     (spit results-file (with-out-str (pp/pprint results)))
     (println (str "Results saved to: " results-file))

     ;; Extract data and render graph
     (let [graph-data (extract-graph-data results)]
       (render-graph graph-data graph-file))

     ;; Also print to console
     (pp/pprint results))))

;; example result:

; (bench true 1)
; ;=>
; {0
;  {clojure.core/doseq
;   {:overhead 9.454425691118012E-9,
;    :samples (56281295 59242814 64708038 70210207 68551620 60602357),
;    :runtime-details
;    {:spec-vendor "Oracle Corporation",
;     :spec-name "Java Virtual Machine Specification",
;     :vm-version "25.0.1",
;     :name "1813363@user-VMware-Virtual-Platform",
;     :clojure-version-string "1.11.3",
;     :java-runtime-version "25.0.1",
;     :java-version "25.0.1",
;     :vm-name "OpenJDK 64-Bit Server VM",
;     :vm-vendor "Homebrew",
;     :clojure-version
;     {:major 1, :minor 11, :incremental 3, :qualifier nil},
;     :spec-version "25",
;     :sun-arch-data-model "64",
;     :input-arguments
;     ["-Dfile.encoding=UTF-8"
;      "-Xms64m"
;      "-Xmx64m"
;      "-Xms2g"
;      "-Xmx2g"
;      "-Djdk.attach.allowAttachSelf"
;      "-Dclojure.compile.path=/home/user/Projects/fully-satisfies-local-dev/bench-doseq/target/classes"
;      "-Dfully-satisfies.version=1.12.1-SNAPSHOT"
;      "-Dclojure.debug=false"]},
;    :mean
;    [1.0301420265151516E-4 (9.673693722943724E-5 1.101027408008658E-4)],
;    :final-gc-time 41789153,
;    :execution-count 616,
;    :variance
;    [9.155321343965616E-11
;     (3.8530578083641504E-11 1.424207273493701E-10)],
;    :os-details
;    {:arch "amd64",
;     :available-processors 2,
;     :name "Linux",
;     :version "6.14.0-37-generic"},
;    :tail-quantile 0.025,
;    :outlier-variance 0.15828197892773935,
;    :outliers {:low-severe 0, :low-mild 0, :high-mild 0, :high-severe 0},
;    :warmup-time 5053749364,
;    :lower-q
;    [9.136573863636364E-5 (9.136573863636364E-5 9.617339935064935E-5)],
;    :warmup-executions 38937,
;    :sample-count 6,
;    :upper-q
;    [1.1286109719967533E-4
;     (1.0504551623376625E-4 1.1397760876623378E-4)],
;    :total-time 0.379596331,
;    :sample-variance [7.922203722039067E-11 (0.0 0.0)],
;    :options
;    {:max-gc-attempts 100,
;     :samples 6,
;     :target-execution-time 100000000,
;     :warmup-jit-period 5000000000,
;     :tail-quantile 0.025,
;     :bootstrap-size 500,
;     :overhead 9.454425691118012E-9},
;    :form
;    (clojure.core/doseq
;     [x
;      (clojure.core/range 10)
;      y
;      (clojure.core/range 10)
;      z
;      (clojure.core/range 10)]
;     (clojure.core/str x y z)),
;    :sample-mean
;    [1.0270463501082251E-4 (7.600260634288201E-5 1.29406663678763E-4)],
;    :results (nil nil nil nil nil nil)},
;   io.github.frenchy64.fully-satisfies.linear-expansion/doseq
;   {:overhead 9.454425691118012E-9,
;    :samples (104656268 94303173 130220185 169265596 95944411 93541648),
;    :runtime-details
;    {:spec-vendor "Oracle Corporation",
;     :spec-name "Java Virtual Machine Specification",
;     :vm-version "25.0.1",
;     :name "1813363@user-VMware-Virtual-Platform",
;     :clojure-version-string "1.11.3",
;     :java-runtime-version "25.0.1",
;     :java-version "25.0.1",
;     :vm-name "OpenJDK 64-Bit Server VM",
;     :vm-vendor "Homebrew",
;     :clojure-version
;     {:major 1, :minor 11, :incremental 3, :qualifier nil},
;     :spec-version "25",
;     :sun-arch-data-model "64",
;     :input-arguments
;     ["-Dfile.encoding=UTF-8"
;      "-Xms64m"
;      "-Xmx64m"
;      "-Xms2g"
;      "-Xmx2g"
;      "-Djdk.attach.allowAttachSelf"
;      "-Dclojure.compile.path=/home/user/Projects/fully-satisfies-local-dev/bench-doseq/target/classes"
;      "-Dfully-satisfies.version=1.12.1-SNAPSHOT"
;      "-Dclojure.debug=false"]},
;    :mean
;    [1.2107110497045535E-4 (1.025039457768509E-4 1.561448284671533E-4)],
;    :final-gc-time 38449428,
;    :execution-count 959,
;    :variance
;    [1.0142575187894027E-9
;     (2.0802450996557704E-10 1.858033826619747E-9)],
;    :os-details
;    {:arch "amd64",
;     :available-processors 2,
;     :name "Linux",
;     :version "6.14.0-37-generic"},
;    :tail-quantile 0.025,
;    :outlier-variance 0.6499396290407803,
;    :outliers {:low-severe 0, :low-mild 0, :high-mild 0, :high-severe 0},
;    :warmup-time 5191275348,
;    :lower-q
;    [9.754082168925966E-5 (9.754082168925966E-5 9.83349040667362E-5)],
;    :warmup-executions 43691,
;    :sample-count 6,
;    :upper-q
;    [1.6808074035453598E-4
;     (1.0913062356621482E-4 1.7650218561001045E-4)],
;    :total-time 0.687931281,
;    :sample-variance [9.861296376753518E-10 (0.0 0.0)],
;    :options
;    {:max-gc-attempts 100,
;     :samples 6,
;     :target-execution-time 100000000,
;     :warmup-jit-period 5000000000,
;     :tail-quantile 0.025,
;     :bootstrap-size 500,
;     :overhead 9.454425691118012E-9},
;    :form
;    (io.github.frenchy64.fully-satisfies.linear-expansion/doseq
;     [x
;      (clojure.core/range 10)
;      y
;      (clojure.core/range 10)
;      z
;      (clojure.core/range 10)]
;     (clojure.core/str x y z)),
;    :sample-mean
;    [1.1955705265901983E-4
;     (2.5348949296951406E-5 2.1376515602108823E-4)],
;    :results (nil nil nil nil nil nil)}}}

;; Test function with stub data
(defn test-graph
  "Test the graph rendering with stub data matching the format above."
  []
  (let [stub-results
        [{0 {`exponential/doseq
             {:mean [1.0E-5 [9.0E-6 1.1E-5]]}
             `linear/doseq
             {:mean [1.2E-5 [1.0E-5 1.4E-5]]}}}
         {1 {`exponential/doseq
             {:mean [5.0E-5 [4.5E-5 5.5E-5]]}
             `linear/doseq
             {:mean [5.2E-5 [4.7E-5 5.7E-5]]}}}
         {2 {`exponential/doseq
             {:mean [2.5E-4 [2.2E-4 2.8E-4]]}
             `linear/doseq
             {:mean [2.6E-4 [2.3E-4 2.9E-4]]}}}
         {3 {`exponential/doseq
             {:mean [1.2E-3 [1.0E-3 1.4E-3]]}
             `linear/doseq
             {:mean [1.25E-3 [1.05E-3 1.45E-3]]}}}
         {4 {`exponential/doseq
             {:mean [6.0E-3 [5.5E-3 6.5E-3]]}
             `linear/doseq
             {:mean [6.1E-3 [5.6E-3 6.6E-3]]}}}
         {5 {`exponential/doseq
             {:mean [3.0E-2 [2.8E-2 3.2E-2]]}
             `linear/doseq
             {:mean [3.05E-2 [2.85E-2 3.25E-2]]}}}]
        graph-data (extract-graph-data stub-results)
        output-dir "bench-results"
        output-file (str output-dir "/test-benchmark-graph.html")]
    ;; Create output directory if it doesn't exist
    (io/make-parents output-file)
    (println "Extracted graph data:")
    (pp/pprint graph-data)
    (render-graph graph-data output-file)
    (println (str "\nTo view the graph, open " output-file " in your browser"))))

;; Usage:
#_
(test-graph)  ; Test with stub data
;; (bench true 5) ; Run quick benchmarks with 5 iterations
