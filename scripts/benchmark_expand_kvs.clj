(ns benchmark-expand-kvs
  (:require [criterium.core :as c]
            [clojure.pprint :as pp]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [io.github.frenchy64.fully-satisfies.expand-kvs :refer [flatten-trailing-map]]))

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
    (io/make-parents output-file)
    (spit output-file html)))

;; approach 1:
;; - use flatten-trailing-map to flatten args inside spec-checker-approach1
;; approach 2:
;; - use an intermediate function `invoke-dispatch` and only flatten args in the rest arity

(defn the-function-being-checked
  ([] [])
  ([a] [a])
  ([a b] [a b])
  ([a b c] [a b c])
  ([a b c d] [a b c d])
  ([a b c d & m] [a b c d m]))

;;simulate spec-checking-fn
(let [the-function-being-checked the-function-being-checked]
  (defn spec-checker-approach1
    [& args]
    (doall (flatten-trailing-map 4 args)) ;; flatten args and walk like `conform!`
    (.applyTo ^clojure.lang.IFn the-function-being-checked args)))

(let [the-function-being-checked the-function-being-checked]
  (defn spec-checker-approach2
    [& args]
    (doall args) ;;walk args like `conform!`
    (.applyTo ^clojure.lang.IFn the-function-being-checked args)))

;; approach2 entry point
(let [spec-checker-approach2 spec-checker-approach2]
  (defn invoke-dispatch
    ([] (spec-checker-approach2))
    ([a] (spec-checker-approach2 a))
    ([a b] (spec-checker-approach2 a b))
    ([a b c] (spec-checker-approach2 a b c))
    ([a b c d] (spec-checker-approach2 a b c d))
    ([a b c d & m] (apply spec-checker-approach2 a b c d (flatten-trailing-map 0 m)))))

(def cases
  [{:input []
    :expected []}
   {:input [1]
    :expected [1]}
   {:input [1 2]
    :expected [1 2]}
   {:input [1 2 3]
    :expected [1 2 3]}
   {:input [1 2 3 4]
    :expected [1 2 3 4]}
   {:input [1 2 3 4 (sorted-map :a 1 :b 2)]
    :expected [1 2 3 4 [:a 1 :b 2]]}
   {:input [1 2 3 4 :a 1 (sorted-map :b 2)]
    :expected [1 2 3 4 [:a 1 :b 2]]}
   {:input [1 2 3 4 :a 1 :b 2]
    :expected [1 2 3 4 [:a 1 :b 2]]}
   {:input [1 2 3 4 :a 1 :b 2 {:c 3}]
    :expected [1 2 3 4 [:a 1 :b 2 :c 3]]}
   {:input [1 2 3 4 :a 1 :b 2 :c 3]
    :expected [1 2 3 4 [:a 1 :b 2 :c 3]]}
   {:input [1 2 3 4 :a 1 :b 2 :c 3 {:d 4}]
    :expected [1 2 3 4 [:a 1 :b 2 :c 3 :d 4]]}
   {:input [1 2 3 4 :a 1 :b 2 :c 3 :d 4]
    :expected [1 2 3 4 [:a 1 :b 2 :c 3 :d 4]]}
   {:input [1 2 3 4 :a 1 :b 2 :c 3 :d 4 {:e 5}]
    :expected [1 2 3 4 [:a 1 :b 2 :c 3 :d 4 :e 5]]}
   {:input [1 2 3 4 :a 1 :b 2 :c 3 :d 4 :e 5]
    :expected [1 2 3 4 [:a 1 :b 2 :c 3 :d 4 :e 5]]}
   {:input [1 2 3 4 :a 1 :b 2 :c 3 :d 4 :e 5 {:f 6}]
    :expected [1 2 3 4 [:a 1 :b 2 :c 3 :d 4 :e 5 :f 6]]}
   {:input [1 2 3 4 :a 1 :b 2 :c 3 :d 4 :e 5 :f 6]
    :expected [1 2 3 4 [:a 1 :b 2 :c 3 :d 4 :e 5 :f 6]]}
   {:input [1 2 3 4 :a 1 :b 2 :c 3 :d 4 :e 5 :f 6 {:g 7}]
    :expected [1 2 3 4 [:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7]]}
   {:input [1 2 3 4 :a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7]
    :expected [1 2 3 4 [:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7]]}])

(assert (apply distinct? (map (comp count :input) cases)))

(def quick? false)

(defn bench* []
  (let [bench-fn (if quick? c/quick-benchmark* c/benchmark*)]
    (into (sorted-map)
          (map (fn [{:keys [input] :as case}]
                 (let [approach1 (eval `(fn [] (spec-checker-approach1 ~@input)))
                       approach2 (eval `(fn [] (invoke-dispatch ~@input)))
                       nargs (count input)
                       fixed? (<= nargs 4)
                       trailing? (and (not fixed?) (odd? nargs))]
                   {[nargs (if fixed? :fixed :rest) (if trailing? :trailing :no-trailing)]
                    (sorted-map
                      :approach1 (do (println "Benchmarking approach1 with" input)
                                     (doto (bench-fn approach1 {}) pp/pprint))
                      :approach2 (do (println "Benchmarking approach2 with" input)
                                     (doto (bench-fn approach2 {}) pp/pprint)))})))
          cases)))

(comment
  (c/quick-bench (apply spec-checker (flatten-trailing-map 4 [])))
  (invoke-dispatch)
  (invoke-dispatch 1)
  (spec-checker)
  (spec-checker 1)

  ;; Generate plots after benchmarks
  (generate-plots)

  )

(defn regen-mean [results-file mean-file]
  (spit mean-file
        (with-out-str
          (let [first-iteration (atom true)]
            (doseq [[info {:keys [approach1 approach2]}] (read-string (slurp results-file))]
              (when-not (first (reset-vals! first-iteration false))
                (println))
              (println info :approach1)
              (c/report-point-estimate "Execution time mean" (:mean approach1))
              (println)
              (println info :approach2)
              (c/report-point-estimate "Execution time mean" (:mean approach2)))))))

(defn regen-pretty [results-file pretty-file]
  (spit pretty-file
        (with-out-str
          (let [first-iteration (atom true)]
            (doseq [[info {:keys [approach1 approach2]}] (read-string (slurp results-file))]
              (when-not (first (reset-vals! first-iteration false))
                (println))
              (println info :approach1)
              (c/report-result approach1)
              (println)
              (println info :approach2)
              (c/report-result approach2))))))

(defn bench []
  (println "Starting benchmarks...")
  (let [results (bench*)
        output-dir "bench-results"
        results-file (str output-dir "/bench-expand-kvs-results.txt")
        pretty-file (str output-dir "/bench-expand-kvs-pretty.txt")
        mean-file (str output-dir "/bench-expand-kvs-mean.txt")]
    (io/make-parents results-file)
    (spit results-file
          (with-out-str
            (pp/pprint results)))
    (println (str "Results saved to: " results-file))
    (regen-pretty results-file pretty-file)
    (println (str "Pretty output saved to: " pretty-file))
    (regen-mean results-file mean-file)
    (println (str "Mean output saved to: " mean-file))))

(defn generate-plots
  "Generate HTML visualizations from benchmark results."
  []
  (let [output-dir "bench-results"
        results-file (str output-dir "/bench-expand-kvs-results.txt")
        results (read-string (slurp results-file))
        values (mapcat (fn [[[nargs] {:keys [approach1 approach2]}]]
                         (assert (nat-int? nargs) (pr-str nargs))
                         [{:nargs nargs
                           :time (* (first (:mean approach1)) 1e9) ; convert to nanoseconds
                           :approach "approach1"}
                          {:nargs nargs
                           :time (* (first (:mean approach2)) 1e9)
                           :approach "approach2"}])
                       results)]
    (export-vega-html!
     {:data {:values values}
      :title "expand-kvs Approach Comparison"
      :width 600
      :height 400
      :mark {:type "line" :point true}
      :encoding {:x {:field "nargs" :type "quantitative" :title "Number of arguments"}
                 :y {:field "time" :type "quantitative" :title "Mean execution time (ns)"}
                 :color {:field "approach" :type "nominal" :title "Approach"}
                 :tooltip [{:field "nargs" :type "quantitative"}
                           {:field "time" :type "quantitative" :format ".4f"}
                           {:field "approach" :type "nominal"}]}}
     (str output-dir "/bench-expand-kvs.html"))
    (println (str "Plot saved to " output-dir "/bench-expand-kvs.html"))))

(comment
  ;; Generate plots after benchmarks
  (generate-plots))
