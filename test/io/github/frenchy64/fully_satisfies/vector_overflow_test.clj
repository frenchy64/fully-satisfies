(ns io.github.frenchy64.fully-satisfies.vector-overflow-test
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts
             :refer [deftest testing]]))

;https://ask.clojure.org/index.php/11080/get-find-assoc-vectors-overflows-key-when-passed-large-longs
(deftest vector-overflow-test
  (let [v [::first]
        wrap-zero 4294967296]
    (is (= (get v wrap-zero)
           (get v wrap-zero :default)
           (get (transient v) wrap-zero)
           (get (transient v) wrap-zero :default)
           (v wrap-zero)
           ::first))
    (is (contains? v wrap-zero))
    (is (contains? (transient v) wrap-zero))
    (is (= (assoc v wrap-zero :wow)
           [:wow]))
    (is (= (persistent! (assoc! (transient v) wrap-zero :wow))
           [:wow]))
    (is (= (find v wrap-zero)
           [wrap-zero ::first]))
    (is (= (find (transient v) wrap-zero)
           [wrap-zero ::first]))))
