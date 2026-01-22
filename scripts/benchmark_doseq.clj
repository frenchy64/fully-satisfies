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
;; (let [v (volatile! nil)
;;   (alter-var-root #'strong (constantly v))
;;   (fn []
;;     (doseq [x0 (range 10)
;;             ...
;;             xn (range 10)]
;;       (str x0 ... xn)))

(defonce strong (volatile! nil))

(defn bench-cases [size]
  (let [syms (mapv (fn [i] (symbol (str "x" i))) (range size))
        binder (vec (interleave syms (repeat `(range 10))))
        body (list* `str syms)
        op->form (fn [op]
                   (list `let ['vol `strong]
                         (list `fn [] (list op binder `(vreset! ~'vol ~body)))))]
    (into (sorted-map)
          (map (fn [op] {op (op->form op)}))
          [`exponential/doseq
           `linear/doseq])))

(comment
  (bench-cases 1)
  (bench-cases 0)
  (bench-cases 2)
  )

(defn bench* [quick? size]
  (let [bench-fn (if quick? c/quick-benchmark* c/benchmark*)
        form->runner (fn [form]
                       (prn "form->runner" form)
                       (eval form))
        opts {:warmup-jit-period 0}]
    (into (sorted-map)
          (map (fn [[op form]]
                 {op (let [runner (form->runner form)]
                       (assert (not (Thread/interrupted)))
                       (assoc (bench-fn runner opts)
                              :form form))}))
          (bench-cases size))))

(defn bench
  ([] (bench false))
  ([quick?] (bench quick? 4))
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

     ;; Also print to console
     (pp/pprint results)
     (prn)
     (prn "========")
     (prn "Summary")
     (prn "========")
     (prn)
     (pp/pprint (mapv #(update-vals % (fn [{[mean] :mean
                                            [variance] :variance}]
                                        {:mean mean
                                         :variance variance}))
                      results)))))

(comment
  (str (.toPlainString (BigDecimal. (double 3.766526703966116E-15))))
  )
