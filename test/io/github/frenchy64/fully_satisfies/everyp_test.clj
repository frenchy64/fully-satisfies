(ns io.github.frenchy64.fully-satisfies.everyp-test
  (:refer-clojure :exclude [every-pred])
  (:require [clojure.test :refer [is]]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [io.github.frenchy64.fully-satisfies.never :refer [never?]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [testing deftest]]
            [io.github.frenchy64.fully-satisfies.everyp :refer [everyp every-pred]]))

(defn everyp-reference [& ps]
  (fn [& args] (every? #(every? % args) ps)))

;; TODO order of operations
;; TODO test zero arity
#_ ;;FIXME
(deftest everyp-test
  (doseq [everyp [everyp every-pred everyp-reference]]
    (testing everyp
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
          (is (true? (apply (apply everyp ps) vs)))))
      (testing "no match"
        (doseq [ret-gen [[false]
                         [nil]
                         [false nil]]
                pred-returns (map (fn [i] (repeatedly i #(rand-nth ret-gen)))
                                  (range 1 7))
                args (map (fn [i] (range i))
                          (range 6))]
          (is (false? (apply (apply everyp (map constantly pred-returns))
                             args))))))))
