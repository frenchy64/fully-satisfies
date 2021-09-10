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
; (out) Evaluation count : 2879580 in 60 samples of 47993 calls.
; (out)              Execution time mean : 21.268488 µs
; (out)     Execution time std-deviation : 450.608411 ns
; (out)    Execution time lower quantile : 20.771101 µs ( 2.5%)
; (out)    Execution time upper quantile : 21.899994 µs (97.5%)
; (out)                    Overhead used : 7.621916 ns
; (out) 
; (out) Found 1 outliers in 60 samples (1.6667 %)
; (out) 	low-severe	 1 (1.6667 %)
; (out)  Variance from outliers : 9.4198 % Variance is slightly inflated by outliers


;; bench io.github.frenchy64.fully-satisfies/fully-satisfies?
; (out) Evaluation count : 38438460 in 60 samples of 640641 calls.
; (out)              Execution time mean : 1.587878 µs
; (out)     Execution time std-deviation : 36.581023 ns
; (out)    Execution time lower quantile : 1.550656 µs ( 2.5%)
; (out)    Execution time upper quantile : 1.670501 µs (97.5%)
; (out)                    Overhead used : 7.621916 ns
; (out) 
; (out) Found 2 outliers in 60 samples (3.3333 %)
; (out) 	low-severe	 2 (3.3333 %)
; (out)  Variance from outliers : 10.9988 % Variance is moderately inflated by outliers

