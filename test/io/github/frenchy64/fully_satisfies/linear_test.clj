(ns io.github.frenchy64.fully-satisfies.linear-test
  (:require [clojure.test :refer [is]]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [testing deftest]]
            [io.github.frenchy64.fully-satisfies.linear :refer [butlast+last
                                                                count+last]]))

(def butlast+last-reference (juxt butlast last))

(deftest butlast+last-test
  (doseq [error-input [nil [] '()]]
    (testing error-input
      (is (thrown? AssertionError (butlast+last error-input)))))
  (doseq [[input expected]
          {[1] [nil 1]
           [1 2] [[1] 2]
           [1 2 3] [[1 2] 3]}]
    (is (= expected
           (butlast+last-reference input)
           (butlast+last input)))))

(def count+last-reference (juxt count last))

(deftest count+last-test
  (doseq [test-case
          {[] [0 nil]
           [42] [1 42]
           [42 43] [2 43]
           [42 43 44] [3 44]}
          mod-input [identity #(lazy-seq %)] ;; counted and uncounted
          :let [[input expected :as test-case] (update test-case 0 mod-input)]]
    (testing test-case
      (let [result (count+last input)]
        (is (instance? Integer (first result)))
        (is (= expected
               result))
        (is (= (count+last-reference input)
               result))))))
