(ns benchmark-expand-kvs
  (:require [criterium.core :as c]
            [io.github.frenchy64.fully-satisfies.expand-kvs :refer [flatten-trailing-map]]))

(defn fixed-arities
  ([] [])
  ([a] [a])
  ([a b] [a b])
  ([a b c] [a b c])
  ([a b c d] [a b c d])
  ([a b c d & m] [a b c d m]))

;;simulate spec-checking-fn
(let [fixed-arities fixed-arities]
  (defn apply-fixed-arities
    [& args]
    (doall args) ;;walk args like `conform!`
    (.applyTo ^clojure.lang.IFn fixed-arities args)))

(let [apply-fixed-arities apply-fixed-arities]
  (defn invoke-dispatch
    ([] (apply-fixed-arities))
    ([a] (apply-fixed-arities a))
    ([a b] (apply-fixed-arities a b))
    ([a b c] (apply-fixed-arities a b c))
    ([a b c d] (apply-fixed-arities a b c d))
    ([a b c d & m] (apply apply-fixed-arities a b c d (flatten-trailing-map 0 m)))))

(def cases
  [{:input []
    :expected []}
   ;{:input [1]
   ; :expected [1]}
   ;{:input [1 2]
   ; :expected [1 2]}
   ;{:input [1 2 3]
   ; :expected [1 2 3]}
   ;{:input [1 2 3 4]
   ; :expected [1 2 3 4]}
   ;{:input [1 2 3 4 :a 1 :b 2]
   ; :expected [1 2 3 4 [:a 1 :b 2]]}
   ;{:input [1 2 3 4 {:a 1}]
   ; :expected [1 2 3 4 [:a 1]]}
   ;{:input [1 2 3 4 :a 1 {:b 2}]
   ; :expected [1 2 3 4 [:a 1 :b 2]]}
   ])

(defn bench-flatten-trailing-map []
  (c/bench
    (doseq [{:keys [input expected] :as case} cases]
      (assert (= expected (apply fixed-arities (flatten-trailing-map 4 input)))
              (pr-str case)))))

(defn bench-invoke-dispatch []
  (c/bench
    (doseq [{:keys [input expected] :as case} cases]
      (assert (= expected (invoke-dispatch input))
              (pr-str case)))))

(defn bench []
  (println "Starting benchmarks...")
  (spit "bench-results.txt"
        (with-out-str
          (println "bench flatten-trailing-map")
          (bench-flatten-trailing-map)
          (println "\n\nbench fixed args")
          (bench-invoke-dispatch))))
