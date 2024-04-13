(ns io.github.frenchy64.fully-satisfies.safer-test
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [io.github.frenchy64.fully-satisfies.safer :as safer]))

(deftest split-at-mutation-test
  (let [up-down (atom true)
        ed (eduction (map (fn [i]
                            (when (zero? i)
                              (swap! up-down not))
                            (if @up-down
                              (- 9 i)
                              i)))
                     (range 10))]
    (is (= [[0 1 2 3 4] [4 3 2 1 0]] (split-at 5 ed)))
    (is (= [[0 1 2 3 4] [5 6 7 8 9]] (safer/split-at 5 ed)))))

(deftest every?-mutation-test
  (let [t-f (atom true)
        ed (eduction (map (fn [i]
                            (when (zero? i)
                              (swap! t-f not))
                            @t-f))
                     (range 10))]
    (is (not (every? identity ed)))
    (is (safer/every? identity ed))))
