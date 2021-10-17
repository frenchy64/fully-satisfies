(ns benchmark-expand-kvs
  (:require [criterium.core :as c]
            [clojure.pprint :as pp]
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
    :expected [1 2 3 4 [:a 1 :b 2]]}])

(assert (apply distinct? (map (comp count :input) cases)))

(def quick? true)

(defn bench* []
  (let [bench-fn (if quick? c/quick-benchmark* c/benchmark*)]
    (into (sorted-map)
          (map (fn [{:keys [input] :as case}]
                 (let [approach1 (eval `(fn [] (spec-checker-approach1 ~@input)))
                       approach2 (eval `(fn [] (invoke-dispatch ~@input)))]
                   {(count input)
                    (sorted-map
                      :approach1 (do (println "Benchmarking approach1 with" input)
                                     (bench-fn approach1 {}))
                      :approach2 (do (println "Benchmarking approach2 with" input)
                                     (bench-fn approach2 {})))})))
          cases)))

(comment
  (c/quick-bench (apply spec-checker (flatten-trailing-map 4 [])))
  (invoke-dispatch)
  (invoke-dispatch 1)
  (spec-checker)
  (spec-checker 1)
  )

(defn bench []
  (println "Starting benchmarks...")
  (let [results (bench*)]
    (spit "bench-expand-kvs-results.txt"
          (with-out-str
            (pp/pprint results)))))
