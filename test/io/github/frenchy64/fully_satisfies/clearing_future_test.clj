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
            [io.github.frenchy64.fully-satisfies.clearing-future :refer [clearing-future
                                                                         clearing-future-call
                                                                         future
                                                                         future-call]])
  (:import [java.util.concurrent Executors ThreadFactory]))

(def ^:dynamic *test-value* 1)

(deftest clearing-future-fn-properly-retains-conveyed-bindings
  (let [a (atom [])]
    (binding [*test-value* 2]
      @(clearing-future
         (dotimes [_ 3]
           ;; we need some binding to trigger binding pop
           (binding [*print-dup* false]
             (swap! a conj *test-value*)))
         ;; after CLJ-2619 no pop is needed
         (swap! a conj *test-value* @(clearing-future *test-value*) @(clearing-future @(clearing-future *test-value*))))
      (is (= (repeat 6 2) @a)))))

(def ^:dynamic *my-var* :root-binding)

;; this test runs a future and then attempts to retrieve the bindings on
;; the clearing-futures thread after the future has finished. it might take a few tries
;; for the second part to work. if it never does, prints a warning instead of failing.
(deftest clearing-future-cleans-up-binding-conveyance
  (doseq [future-impl [:clearing-future
                       :clearing-future-call
                       :future
                       :future-call]]
    (testing future-impl
      (let [try-flaky-test
            (fn []
              (let [global-solo-executor clojure.lang.Agent/soloExecutor
                    thread-count (atom 0)
                    current-thread-prm (promise)
                    local-executor (Executors/newCachedThreadPool
                                     (reify ThreadFactory
                                       (newThread [_ runnable]
                                         (swap! thread-count inc)
                                         (Thread. runnable))))]
                (try
                  (set-agent-send-off-executor! local-executor)
                  (let [f-prm (promise)
                        f (binding [*my-var* :thread-binding
                                    *1 :foo]
                            (let [future-body (fn []
                                                (deliver current-thread-prm (Thread/currentThread))
                                                @f-prm)]
                              (case future-impl
                                :clearing-future (clearing-future (future-body))
                                :future (future (future-body))
                                :clearing-future-call (clearing-future-call future-body)
                                :future-call (clearing-future-call future-body))))
                        _ (set-agent-send-off-executor! global-solo-executor)
                        ;; there's a chance other futures could have seen local-executor, so wait a bit to
                        ;; let them start running. if this happens, since f is blocked it will force another
                        ;; local-executor thread to be created. this will increment thread-count and 
                        ;; trigger a retry on this test.
                        _ (Thread/sleep 200)
                        _ (deliver f-prm :done)
                        _ @f
                        _ (Thread/sleep 200) ;; encourage local-executor to reuse thread
                        after-thread-bindings-prm (promise)
                        result (deref (.submit local-executor
                                               ^Callable
                                               (fn* []
                                                    (if (= @current-thread-prm (Thread/currentThread))
                                                      (get-thread-bindings)
                                                      :wrong-thread)))
                                      5000 ::timeout)
                        thread-count @thread-count]
                    (cond
                      (= 1 thread-count) result
                      ;; something interfered
                      :else :bad-thread-count))
                  (finally
                    (set-agent-send-off-executor! global-solo-executor)
                    (.shutdown local-executor)))))
            flaky-test-result (reduce (fn [results _]
                                        (let [maybe-flaky-result (try-flaky-test)]
                                          (case maybe-flaky-result
                                            ::timeout (throw (ex-info "Timeout" {:results (conj results maybe-flaky-result)}))
                                            ;; retry
                                            (:wrong-thread :bad-thread-count) (conj results maybe-flaky-result)
                                            (reduced maybe-flaky-result))))
                                      []
                                      (range 50))]
        (cond
          (map? flaky-test-result) (let [actual-thread-bindings-after-future flaky-test-result]
                                     (is (= {} actual-thread-bindings-after-future)))

          (every? #{:wrong-thread :bad-thread-count} flaky-test-result)
          (println (str "WARNING: " `future-cleans-up-binding-conveyance 
                        " test was very unlucky"
                        " and could not verify thread bindings. "
                        flaky-test-result))

          :else (throw (ex-info "Unknown result" {:result flaky-test-result})))))))
