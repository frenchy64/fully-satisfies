(ns io.github.frenchy64.fully-satisfies.uniform-test
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [io.github.frenchy64.fully-satisfies.uniform :as uniform]))

(deftest partition-by-uniform-test
  (testing "reference"
    (is (= [[1] [nil] [3] [nil]] (into [] (partition-by identity) [1 nil 3 nil])))
    (is (= [[1] [nil] [3] [nil]] (into [] (uniform/partition-by identity) [1 nil 3 nil]))))
  (testing "difference"
    ;; wrong
    (is (= [[1] [:clojure.core/none 3] [:clojure.core/none]]
           (into [] (partition-by identity) [1 :clojure.core/none 3 :clojure.core/none])))
    ;; right
    (is (= [[1] [:clojure.core/none] [3] [:clojure.core/none]]
           (into [] (uniform/partition-by identity) [1 :clojure.core/none 3 :clojure.core/none])))))

(deftest halt-when-uniform-test
  ;; wrong
  (is (= :should-be-wrapped
         (into () (comp (halt-when (fn [_] false))
                        (fn [rf]
                          (fn
                            ([] (rf))
                            ([result] (rf result))
                            ([result input]
                             (reduced {:clojure.core/halt :should-be-wrapped})))))
               [1])))
  ;; right
  (is (= {:clojure.core/halt :should-be-wrapped}
         (into () (comp (uniform/halt-when (fn [_] false))
                        (fn [rf]
                          (fn
                            ([] (rf))
                            ([result] (rf result))
                            ([result input]
                             (reduced {:clojure.core/halt :should-be-wrapped})))))
               [1]))))
