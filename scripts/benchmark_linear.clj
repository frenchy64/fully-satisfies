;; lein bench-linear
(ns benchmark-linear
  (:require [criterium.core :as c]
            [clojure.pprint :as pp]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [io.github.frenchy64.fully-satisfies.linear :refer [butlast+last
                                                                count+last]]
            [io.github.frenchy64.fully-satisfies.linear-test :refer [butlast+last-reference
                                                                     count+last-reference]]))

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

(def quick? true)

(defn bench* []
  (let [bench-fn (if quick? c/quick-benchmark* c/benchmark*)]
    (into (sorted-map)
          (map (fn [size]
                 {:pre [(nat-int? size)]}
                 (let [counted-input (apply list (range size))
                       _ (assert (counted? counted-input))
                       uncounted-input (take size (range))
                       _ (assert (not (counted? uncounted-input)))
                       gen-case (fn [id input-kind]
                                  (let [input (case input-kind
                                                :counted counted-input
                                                :uncounted uncounted-input)
                                        f (case id
                                            :last last
                                            :count count
                                            :butlast butlast
                                            :butlast+last butlast+last
                                            :butlast+last-reference butlast+last-reference
                                            :count+last count+last
                                            :count+last-reference count+last-reference)
                                        k [id input-kind]]
                                    {[id input-kind]
                                     (do (println (format "Benchmarking %s with size %s" k size))
                                         (doto (bench-fn #(f input) {})
                                           pp/pprint))}))]
                   {size
                    (into (sorted-map)
                          (for [id (concat
                                     (when (pos? size)
                                       [:butlast+last
                                        :butlast+last-reference])
                                     [:butlast
                                      :last
                                      :count
                                      :count+last
                                      :count+last-reference])
                                input-kind [:counted :uncounted]]
                            (gen-case id input-kind)))})))
          (list 0
                1
                5
                10
                100
                #_1000
                #_10000
                #_100000))))

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
            (doseq [[info k->result] (read-string (slurp results-file))
                    [k result] k->result]
              (when-not (first (reset-vals! first-iteration false))
                (println))
              (println info k)
              (c/report-point-estimate "Execution time mean" (:mean result)))))))

(defn regen-pretty [results-file pretty-file]
  (spit pretty-file
        (with-out-str
          (let [first-iteration (atom true)]
            (doseq [[info k->result] (read-string (slurp results-file))
                    [k result] k->result]
              (when-not (first (reset-vals! first-iteration false))
                (println))
              (println info k)
              (c/report-result result))))))

(defn bench []
  (println "Starting benchmarks...")
  (let [results (bench*)
        output-dir "bench-results"
        results-file (str output-dir "/bench-linear-results.txt")
        pretty-file (str output-dir "/bench-linear-pretty.txt")
        mean-file (str output-dir "/bench-linear-mean.txt")]
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
        results-file (str output-dir "/bench-linear-results.txt")
        results (read-string (slurp results-file))
        values (mapcat (fn [[size rs]]
                         (assert (nat-int? size) (pr-str size))
                         (map (fn [[k {[mean] :mean}]]
                                {:size size
                                 :time (* mean 1e6) ; convert to microseconds
                                 :f (str k)})
                              rs))
                       results)
        groups (group-by (comp first :f) values)]

    ;; Generate overall comparison plot
    (export-vega-html!
     {:data {:values values}
      :title "Linear Operations Benchmark - All Functions"
      :width 600
      :height 400
      :mark {:type "line" :point true}
      :encoding {:x {:field "size" :type "quantitative" :title "Input size"}
                 :y {:field "time" :type "quantitative" :title "Mean execution time (μs)" :scale {:type "log"}}
                 :color {:field "f" :type "nominal" :title "Function"}
                 :tooltip [{:field "size" :type "quantitative"}
                           {:field "time" :type "quantitative" :format ".4f"}
                           {:field "f" :type "nominal"}]}}
     (str output-dir "/bench-linear-all.html"))

    ;; Generate count+last comparison plot
    (let [count-last-values (concat (get groups :count+last [])
                                    (get groups :count+last-reference [])
                                    (get groups :last [])
                                    (get groups :count []))]
      (export-vega-html!
       {:data {:values count-last-values}
        :title "count+last Family Comparison"
        :width 600
        :height 400
        :mark {:type "line" :point true}
        :encoding {:x {:field "size" :type "quantitative" :title "Input size"}
                   :y {:field "time" :type "quantitative" :title "Mean execution time (μs)" :scale {:type "log"}}
                   :color {:field "f" :type "nominal" :title "Function"}
                   :tooltip [{:field "size" :type "quantitative"}
                             {:field "time" :type "quantitative" :format ".4f"}
                             {:field "f" :type "nominal"}]}}
       (str output-dir "/bench-linear-count-last.html")))

    ;; Generate butlast+last comparison plot
    (let [butlast-last-values (concat (get groups :butlast+last [])
                                      (get groups :butlast+last-reference [])
                                      (get groups :last [])
                                      (get groups :butlast []))]
      (export-vega-html!
       {:data {:values butlast-last-values}
        :title "butlast+last Family Comparison"
        :width 600
        :height 400
        :mark {:type "line" :point true}
        :encoding {:x {:field "size" :type "quantitative" :title "Input size"}
                   :y {:field "time" :type "quantitative" :title "Mean execution time (μs)" :scale {:type "log"}}
                   :color {:field "f" :type "nominal" :title "Function"}
                   :tooltip [{:field "size" :type "quantitative"}
                             {:field "time" :type "quantitative" :format ".4f"}
                             {:field "f" :type "nominal"}]}}
       (str output-dir "/bench-linear-butlast-last.html")))

    (println (str "Plots saved to " output-dir "/"))
    (println "  - bench-linear-all.html")
    (println "  - bench-linear-count-last.html")
    (println "  - bench-linear-butlast-last.html")))

(comment
  ;; Generate plots after benchmarks
  (generate-plots))
