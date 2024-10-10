;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.clearing-future-test
  (:refer-clojure :exclude [future future-call])
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [testing deftest]]
            [io.github.frenchy64.fully-satisfies.clearing-future :refer [future future-call]])
  (:import [java.util.concurrent Executors ThreadFactory]))

(def ^:dynamic *test-value* 1)

(deftest clearing-future-fn-properly-retains-conveyed-bindings
  (let [sentinel (Object.)
        a (atom [])]
    (binding [*test-value* sentinel]
      @(future
         (dotimes [_ 3]
           ;; we need some binding to trigger binding pop
           (binding [*print-dup* false]
             (swap! a conj
                    *test-value*
                    @(future *test-value*)
                    @(future @(future *test-value*))))))
      (is (= (repeat 9 sentinel) @a)))))

;; improve likelihood of catching a Thread holding onto its thread bindings
;; before it's cleared by another job. note this only expands the pool for futures
;; and send-off, not send-via.
(let [pool-size 500
      d (delay (let [p (promise)]
                 (mapv deref (mapv #(future (if (= (dec pool-size) %) (deliver p true) @p)) (range pool-size)))))]
  (defn expand-thread-pool! [] @d nil))

(deftest future-cleans-up-binding-conveyance
  (expand-thread-pool!)
  (let [strong-ref (volatile! (Object.))
        weak-ref (java.lang.ref.WeakReference. @strong-ref)]
    (binding [*test-value* @strong-ref]
      @(future
         (or (identical? @strong-ref *test-value*)
             (throw (Exception.)))))
    (vreset! strong-ref nil)
    (System/gc)
    (doseq [i (range 10)
            :while (some? (.get weak-ref))]
      (Thread/sleep 1000)
      (System/gc))
    (is (nil? (.get weak-ref)))))
