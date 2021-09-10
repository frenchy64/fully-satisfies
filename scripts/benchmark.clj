(ns benchmark
  (:require [criterium.core :as c]
            [clj-async-profiler.core :as p]
            [io.github.frenchy64.fully-satisfies :as sut]))


(defprotocol Foo
  (bar [this a])
  (baz [this b])
  (bax [this c])
  (baw [this d]))

(defrecord Bar []
  Foo
  (bar [this a])
  (baw [this d]))


(comment

  ;; start profiler at port 17042
  (p/serve-files 17042)

  ;; use criterium bench on clojure.core/satisfies?
  (c/with-progress-reporting
    (c/bench
     (satisfies? Foo (Bar.))))

  ;; use criterium bench on fully-satisfies?
  (c/with-progress-reporting
    (c/bench
     (sut/fully-satisfies? Foo (Bar.))))

  (def flamegraph-sample-number 10000)

  ;; execute to generate flamegraph
  ;; goto https://localhost:17042 to see the images
  (p/profile
   (dotimes [_ flamegraph-sample-number]
     (sut/fully-satisfies? Foo (Bar.))))

  )

;; bench clojure.core/satisfies?
;; Evaluation count : 2585077800 in 60 samples of 43084630 calls.
;;              Execution time mean : 16.454595 ns
;;     Execution time std-deviation : 0.358982 ns
;;    Execution time lower quantile : 16.066100 ns ( 2.5%)
;;    Execution time upper quantile : 17.374432 ns (97.5%)
;;                    Overhead used : 6.888735 ns

;; Found 4 outliers in 60 samples (6.6667 %)
;; 	low-severe	 3 (5.0000 %)
;; 	low-mild	 1 (1.6667 %)
;;  Variance from outliers : 9.4529 % Variance is slightly inflated by outliers


;; bench io.github.frenchy64.fully-satisfies/fully-satisfies?
;; Evaluation count : 93682620 in 60 samples of 1561377 calls.
;;              Execution time mean : 664.533753 ns
;;     Execution time std-deviation : 24.724335 ns
;;    Execution time lower quantile : 633.942034 ns ( 2.5%)
;;    Execution time upper quantile : 715.617187 ns (97.5%)
;;                    Overhead used : 7.758689 ns
;; 
;; Found 1 outliers in 60 samples (1.6667 %)
;; 	low-severe	 1 (1.6667 %)
;;  Variance from outliers : 23.8242 % Variance is moderately inflated by outliers
