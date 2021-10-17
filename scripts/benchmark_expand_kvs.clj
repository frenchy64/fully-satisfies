(ns benchmark-expand-kvs
  (:require [criterium.core :as c]
            [clojure.pprint :as pp]
            [oz.core :as oz]
            [io.github.frenchy64.fully-satisfies.expand-kvs :refer [flatten-trailing-map]]))

;; approach 1:
;; - use flatten-trailing-map to flatten args inside spec-checking-fn
;; approach 2:
;; - generate a thunk and only flatten args in the rest arity

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

  (oz/start-server!)

  )

(defn regen-mean []
  (spit "bench-expand-kvs-mean.txt"
        (with-out-str
          (let [first-iteration (atom true)]
            (doseq [[size {:keys [approach1 approach2]}] (read-string (slurp "bench-expand-kvs-results.txt"))]
              (when-not (first (reset-vals! first-iteration false))
                (println))
              (println size :approach1)
              (c/report-point-estimate "Execution time mean" (:mean approach1))
              (println)
              (println size :approach2)
              (c/report-point-estimate "Execution time mean" (:mean approach2)))))))

(defn regen-pretty []
  (spit "bench-expand-kvs-pretty.txt"
        (with-out-str
          (let [first-iteration (atom true)]
            (doseq [[size {:keys [approach1 approach2]}] (read-string (slurp "bench-expand-kvs-results.txt"))]
              (when-not (first (reset-vals! first-iteration false))
                (println))
              (println size :approach1)
              (c/report-result approach1)
              (println)
              (println size :approach2)
              (c/report-result approach2))))))

(defn bench []
  (println "Starting benchmarks...")
  (let [results (bench*)]
    (spit "bench-expand-kvs-results.txt"
          (with-out-str
            (pp/pprint results)))
    (regen-pretty)
    (regen-mean)))

(defn mean-line-plot* []
  (let [results (read-string (slurp "bench-expand-kvs-results.txt"))]
    {:data {:values (mapcat (fn [[k {:keys [approach1 approach2]}]]
                              (let [nargs (if (number? k)
                                            k
                                            (first k))]
                                (assert (nat-int? nargs))
                                [{:nargs nargs
                                  :time (first (:mean approach1))
                                  :approach "approach1"}
                                 {:nargs nargs
                                  :time (first (:mean approach2))
                                  :approach "approach2"}]))
                            results)}
     :encoding {:x {:field "nargs" :type "ordinal"}
                :y {:field "time" :type "quantitative"}
                :color {:field "approach" :type "nominal"}}
     :mark "line"}))

(comment
  (oz/view! (mean-line-plot*)))
