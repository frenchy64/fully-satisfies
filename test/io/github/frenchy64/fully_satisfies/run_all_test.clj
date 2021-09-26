(ns io.github.frenchy64.fully-satisfies.run-all-test
  (:require [clojure.test :refer :all]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [clojure.test.check.generators :as gen]
            [io.github.frenchy64.fully-satisfies.run-all :refer [run-all!]]))

(defn test-run-all! [c]
  (testing c
    (let [at (atom [])
          _ (run-all! #(swap! at conj %) c)]
      (is (= (vec c) @at)))))

(deftest run-all!-test
  (doseq [c [nil
             []
             [:a]
             [1 2 3]
             [1 (reduced 2) 3]
             (range 100)]]
    (test-run-all! c))
  (checking
    "run-all!"
    [c (gen/vector gen/any-equatable)]
    (test-run-all! c)))
