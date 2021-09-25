(ns io.github.frenchy64.fully-satisfies.each-test
  (:require [clojure.test :refer :all]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [io.github.frenchy64.fully-satisfies.each :refer [each]]))

(deftest each-test
  (doseq [c [[]
             [:a]
             [1 2 3]
             (range 100)]]
    (testing c
      (let [at (atom [])
            _ (each #(swap! at conj %) c)]
        (is (= c @at))))))
