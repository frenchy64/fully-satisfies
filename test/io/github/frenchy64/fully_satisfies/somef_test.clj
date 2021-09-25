(ns io.github.frenchy64.fully-satisfies.somef-test
  (:require [clojure.test :refer :all]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [io.github.frenchy64.fully-satisfies.never :refer [never?]]
            [io.github.frenchy64.fully-satisfies.somef :refer [somef]]))

(defn somef-reference [& ps]
  (fn [& args] (some #(some % args) ps)))

;; TODO order of operations
(deftest somef-test
  (doseq [somef [somef somef-reference]]
    (testing "found match"
      (doseq [v [true 1 42 :a 'a]
              vs [[v]
                  [false v]
                  [nil v false]
                  [false nil v]]
              ps [[identity]
                  [never? identity]
                  [identity never?]
                  [never? identity never?]]]
        (is (= v (apply (apply somef ps) vs)))))
    (testing "no match"
      (doseq [ret-gen [[false]
                       [nil]
                       [false nil]]
              pred-returns (map (fn [i] (repeatedly i #(rand-nth ret-gen)))
                                (range 1 7))
              args (map (fn [i] (range i))
                        (range 6))]
        (is (nil? (apply (apply somef (map constantly pred-returns))
                         args)))))))
