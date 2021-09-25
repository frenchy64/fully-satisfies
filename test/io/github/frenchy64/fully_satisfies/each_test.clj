(ns io.github.frenchy64.fully-satisfies.each-test
  (:require [clojure.test :refer :all]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [clojure.test.check.generators :as gen]
            [io.github.frenchy64.fully-satisfies.each :refer [each]]))

(defn test-each [c]
  (testing c
    (let [at (atom [])
          _ (each #(swap! at conj %) c)]
      (is (= (vec c) @at)))))

(deftest each-test
  (doseq [c [nil
             []
             [:a]
             [1 2 3]
             [1 (reduced 2) 3]
             (range 100)]]
    (test-each c))
  (checking
    "each"
    [c (gen/vector gen/any-equatable)]
    (test-each c)))
