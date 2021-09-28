(ns io.github.frenchy64.fully-satisfies.vector-overflow-test
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts
             :refer [deftest testing]]
            [io.github.frenchy64.fully-satisfies.vector-overflow :as vo]))

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

(deftest fixed-vector-overflow-test
  (doseq [[id v] {`vo/vector (vo/vector ::first)
                  `vo/vec (vo/vec [::first])}]
    (testing id
      (let [wrap-zero 4294967296]
        (is (= (get v wrap-zero)
               (get (transient v) wrap-zero)
               (v wrap-zero)
               nil))
        (is (= (get v wrap-zero :default)
               (get (transient v) wrap-zero :default)
               :default))
        (is (not (contains? v wrap-zero)))
        (is (not (contains? (transient v) wrap-zero)))
        (is (= (assoc v wrap-zero :wow)
               ::FIXME))
        (is (= (persistent! (assoc! (transient v) wrap-zero :wow))
               ::FIXME))
        (is (= (find v wrap-zero)
               nil))
        (is (= (find (transient v) wrap-zero)
               nil))))))
