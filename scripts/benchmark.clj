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

(defn bench-satisfies? []
  (c/bench
    (do
      (satisfies? Foo (Bar.))
      (satisfies? PExtendViaMetadata
                  (with-meta {}
                             {`aPExtendViaMetadata (fn [this])}))
      (satisfies? A (->ExtendedAB)))))

(defn bench-fully-satisfies? []
  (c/bench
    (do
      (sut/fully-satisfies? Foo (Bar.))
      (sut/fully-satisfies? PExtendViaMetadata
                            (with-meta {}
                                       {`aPExtendViaMetadata (fn [this])}))
      (sut/fully-satisfies? A (->ExtendedAB)))))

(defn bench []
  (println "Starting benchmarks...")
  (spit "bench-results.txt"
        (with-out-str
          (println "bench satisfies?")
          (bench-satisfies?)
          (println "\n\nbench fully-satisfies?")
          (bench-fully-satisfies?)))
  (println "Finished benchmarking."))

(comment
  ;; see bench-results.txt
  (bench)

  ;; start profiler at port 17042
  (p/serve-files 17042)

  ;; use criterium bench on clojure.core/satisfies?
  (c/with-progress-reporting
    (bench-satisfies?))

  ;; use criterium bench on fully-satisfies?
  (c/with-progress-reporting
    (bench-fully-satisfies?))

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
