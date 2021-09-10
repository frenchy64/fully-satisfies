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

(defprotocol PExtendViaMetadata
  :extend-via-metadata true
  (aPExtendViaMetadata [this])
  (bPExtendViaMetadata [this]))

(defprotocol A
  (aA [this])
  (bA [this]))

(deftype NoExtended [])
(deftype Extended [])
(deftype ExtendedA [])
(deftype ExtendedAB [])
(extend-protocol A
  Extended
  (aA [_])
  ExtendedA
  (aA [_])
  ExtendedAB
  (aA [_])
  (bA [_]))

(comment

  ;; start profiler at port 17042
  (p/serve-files 17042)

  ;; use criterium bench on clojure.core/satisfies?
  (c/with-progress-reporting
    (c/bench
     (do
       (satisfies? Foo (Bar.))
       (satisfies? PExtendViaMetadata
                   (with-meta {}
                     {`aPExtendViaMetadata (fn [this])}))
       (satisfies? A (->ExtendedAB)))))

  ;; use criterium bench on fully-satisfies?
  (c/with-progress-reporting
    (c/bench
     (do
       (sut/fully-satisfies? Foo (Bar.))
       (sut/fully-satisfies? PExtendViaMetadata
                             (with-meta {}
                               {`aPExtendViaMetadata (fn [this])}))
       (sut/fully-satisfies? A (->ExtendedAB)))))

  (def flamegraph-sample-number 10000)

  ;; execute to generate flamegraph
  ;; goto https://localhost:17042 to see the images
  (p/profile
   (dotimes [_ flamegraph-sample-number]
     (sut/fully-satisfies? Foo (Bar.))
     (sut/fully-satisfies? PExtendViaMetadata
                           (with-meta {}
                             {`aPExtendViaMetadata (fn [this])}))
     (sut/fully-satisfies? A (->ExtendedAB))))

  )

;; bench clojure.core/satisfies?
; Evaluation count : 2677800 in 60 samples of 44630 calls.
;              Execution time mean : 22.384349 µs
;     Execution time std-deviation : 622.007472 ns
;    Execution time lower quantile : 21.742259 µs ( 2.5%)
;    Execution time upper quantile : 24.003420 µs (97.5%)
;                    Overhead used : 7.982414 ns
; 
; Found 4 outliers in 60 samples (6.6667 %)
; 	low-severe	 3 (5.0000 %)
; 	low-mild	 1 (1.6667 %)
;  Variance from outliers : 14.2412 % Variance is moderately inflated by outliers


;; bench io.github.frenchy64.fully-satisfies/fully-satisfies?
; (out) Evaluation count : 38765160 in 60 samples of 646086 calls.
; (out)              Execution time mean : 1.660924 µs
; (out)     Execution time std-deviation : 56.536367 ns
; (out)    Execution time lower quantile : 1.566917 µs ( 2.5%)
; (out)    Execution time upper quantile : 1.784531 µs (97.5%)
; (out)                    Overhead used : 7.982414 ns
; (out) 
; (out) Found 1 outliers in 60 samples (1.6667 %)
; (out) 	low-severe	 1 (1.6667 %)
; (out)  Variance from outliers : 20.6116 % Variance is moderately inflated by outliers
