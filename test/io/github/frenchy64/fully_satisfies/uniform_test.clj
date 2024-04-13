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
