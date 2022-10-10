(ns io.github.frenchy64.fully-satisfies.cleaners-test
  "Goal: use Java Cleaners to test for memory leaks"
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [testing deftest]]
            [io.github.frenchy64.fully-satisfies.cleaners :refer [cleaner try-forcing-cleaners!]]))

(deftest try-forcing-cleaners!-test
  (let [cleaned? (atom [])
        _ (doto (volatile! (cleaner #(swap! cleaned? conj true)))
            (vreset! nil))]
    (try-forcing-cleaners! 1000)))

(deftest reduce3-processes-sequentially-test
  (let [a (atom 0)
        c (repeatedly 2 #(hash-map :cleaner (cleaner (partial swap! a inc))))]
    (reduce (fn [i c]
              (let [i (case (int i)
                        0 (do (is (= 0 @a))
                              (inc i))
                        1 (do (try-forcing-cleaners!)
                              (is (= 1 @a))))]
                (print "Printing cleaner as evidence" c)
                i))
            0
            c)))
