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
;; Evaluation count : 4958280 in 60 samples of 82638 calls.
;;              Execution time mean : 12.067375 µs
;;     Execution time std-deviation : 311.210275 ns
;;    Execution time lower quantile : 11.791342 µs ( 2.5%)
;;    Execution time upper quantile : 12.908302 µs (97.5%)
;;                    Overhead used : 6.883453 ns


;; bench io.github.frenchy64.fully-satisfies/fully-satisfies?
;; Evaluation count : 41180040 in 60 samples of 686334 calls.
;;              Execution time mean : 1.494673 µs
;;     Execution time std-deviation : 18.077293 ns
;;    Execution time lower quantile : 1.461525 µs ( 2.5%)
;;    Execution time upper quantile : 1.527422 µs (97.5%)
;;                    Overhead used : 6.883453 ns
