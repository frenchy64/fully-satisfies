(ns io.github.frenchy64.fully-satisfies.run-all-test
  (:refer-clojure :exclude [run!])
  (:require [clojure.test :refer [is]]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [clojure.test.check.generators :as gen]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [testing deftest]]
            [io.github.frenchy64.fully-satisfies.run-all :refer [run-all! run!]]))

(defn test-run-all! [c]
  (testing c
    (doseq [run-all! [run-all! run!]]
      (testing run-all!
        (let [at (atom [])
              _ (run-all! #(swap! at conj %) c)]
          (is (= (vec c) @at)))))))

(deftest run-all!-test
  (doseq [c [nil
             []
             [:a]
             [1 2 3]
             (sorted-set 1 2 3)
             (sorted-map 1 2 3 4)
             [1 (reduced 2) 3]
             (range 100)]]
    (test-run-all! c))
  (checking
    "run-all!"
    [c (gen/vector gen/any-equatable)]
    (test-run-all! c)))
