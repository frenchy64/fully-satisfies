(ns io.github.frenchy64.fully-satisfies.cleaners-test
  "Goal: use Java Cleaners to test for memory leaks"
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [testing deftest]]
            [io.github.frenchy64.fully-satisfies.cleaners :refer [register-cleaner! try-forcing-cleaners!]]))

(deftest try-forcing-cleaners!-test
  (let [cleaned? (atom [])
        _ (doto (volatile! (register-cleaner! (Object.) #(swap! cleaned? conj true)))
            (vreset! nil))]
    (try-forcing-cleaners!)))

(deftest reduce3-processes-sequentially-test
  (let [a (atom 0)
        c (repeatedly 10 #(doto (hash-map :something (rand))
                            (register-cleaner! (fn [] (swap! a inc)))))]
    (reduce (fn [i c]
              (let [i (do (try-forcing-cleaners!)
                          (is (= i @a))
                          (inc i))]
                (println "Printing cleaner as evidence" c)
                i))
            0
            c)
    (try-forcing-cleaners!)
    (is (= 10 @a))))
