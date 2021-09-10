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


;;
;;
;; bench io.github.frenchy64.fully-satisfies/fully-satisfies?
;; Evaluation count : 6195060 in 60 samples of 103251 calls.
;;              Execution time mean : 9.797970 µs
;;     Execution time std-deviation : 406.875147 ns
;;    Execution time lower quantile : 9.635392 µs ( 2.5%)
;;    Execution time upper quantile : 10.126902 µs (97.5%)
;;                    Overhead used : 6.888735 ns

;; Found 3 outliers in 60 samples (5.0000 %)
;; 	low-severe	 1 (1.6667 %)
;; 	low-mild	 2 (3.3333 %)
;;  Variance from outliers : 28.6409 % Variance is moderately inflated by outliers
