;; lein bench-linear
(ns benchmark-linear
  (:require [criterium.core :as c]
            [clojure.pprint :as pp]
            [oz.core :as oz]
            [io.github.frenchy64.fully-satisfies.linear :refer [butlast+last
                                                                count+last]]
            [io.github.frenchy64.fully-satisfies.linear-test :refer [butlast+last-reference
                                                                     count+last-reference]]))

(def quick? true)

(defn bench* []
  (let [bench-fn (if quick? c/quick-benchmark* c/benchmark*)]
    (into (sorted-map)
          (map (fn [size]
                 {:pre [(nat-int? size)]}
                 (let [counted-input (into [] (range size))
                       _ (assert (counted? counted-input))
                       uncounted-input (take size (range))
                       _ (assert (not (counted? uncounted-input)))
                       gen-case (fn [id input-kind]
                                  (let [input (case input-kind
                                                :counted counted-input
                                                :uncounted uncounted-input)
                                        f (case id
                                            :butlast+last butlast+last
                                            :butlast+last-reference butlast+last-reference
                                            :count+last count+last
                                            :count+last-reference count+last-reference)
                                        k [id input-kind]]
                                    {[id input-kind]
                                     (do (println (format "Benchmarking %s with size %s" k size))
                                         (doto (bench-fn #(f uncounted-input) {})
                                           pp/pprint))}))]
                   {size
                    (into (sorted-map)
                          (for [id (concat
                                     (when (pos? size)
                                       [:butlast+last
                                        :butlast+last-reference])
                                     [:count+last
                                      :count+last-reference])
                                input-kind [:counted :uncounted]]
                            (gen-case id input-kind)))})))
          (list 0
                1
                5
                10
                100
                1000
                10000
                100000))))

(comment
  (c/quick-bench (apply spec-checker (flatten-trailing-map 4 [])))
  (invoke-dispatch)
  (invoke-dispatch 1)
  (spec-checker)
  (spec-checker 1)

  (oz/start-server!)

  )

(defn regen-mean []
  (spit "bench-linear-mean.txt"
        (with-out-str
          (let [first-iteration (atom true)]
            (doseq [[info k->result] (read-string (slurp "bench-linear-results.txt"))
                    [k result] k->result]
              (when-not (first (reset-vals! first-iteration false))
                (println))
              (println info k)
              (c/report-point-estimate "Execution time mean" (:mean result)))))))

(defn regen-pretty []
  (spit "bench-linear-pretty.txt"
        (with-out-str
          (let [first-iteration (atom true)]
            (doseq [[info k->result] (read-string (slurp "bench-linear-results.txt"))
                    [k result] k->result]
              (when-not (first (reset-vals! first-iteration false))
                (println))
              (println info k)
              (c/report-result result))))))

(defn bench []
  (println "Starting benchmarks...")
  (let [results (bench*)]
    (spit "bench-linear-results.txt"
          (with-out-str
            (pp/pprint results)))
    (regen-pretty)
    (regen-mean)))

(defn plots* []
  (let [results (read-string (slurp "bench-linear-results.txt"))
        values (mapcat (fn [[size rs]]
                         (assert (nat-int? size) (pr-str size))
                         (map (fn [[k {[mean] :mean}]]
                                {:size size
                                 :time mean
                                 :f k})
                              rs))
                       results)
        groups (group-by (comp {:count+last :count+last
                                :count+last-reference :count+last
                                :butlast+last :butlast+last
                                :butlast+last-reference :butlast+last}
                               first
                               :f) values)]
    (map (fn [[k values]]
           (assert (keyword? k) (pr-str k))
           [:div
            [:h1 (name k)]
            (map (fn [limit]
                   [:vega-lite
                    {:data {:values (filter #(<= (:size %) limit) values)}
                     :encoding {:x {:field "size" :type "ordinal"}
                                :y {:field "time" :type "quantitative"}
                                :color {:field "f" :type "nominal"}}
                     :mark "line"}])
                 [10 100 1000 1000000])])
         groups)))

(comment
  (oz/start-server!)
  (oz/view! (plots*)))
