(ns io.github.frenchy64.fully-satisfies.safer-test
  (:refer-clojure :exclude [splitv-at])
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
    (reset! up-down true)
    (is (= [[0 1 2 3 4] [5 6 7 8 9]] (safer/split-at 5 ed)))))

(deftest split-with-mutation-test
  (let [t-f (atom false)
        ed (eduction (map (fn [i]
                            (when (zero? i)
                              (swap! t-f not))
                            @t-f))
                     (range 10))]
    (is (= [[true true true true true true true true true true]
            [false false false false false false false false false false]]
           (split-with true? ed)))
    (reset! t-f false)
    (is (= [[true true true true true true true true true true] []]
           (safer/split-with true? ed)))))

(deftest every?-mutation-test
  (let [t-f (atom false)
        ed (eduction (map (fn [i]
                            (when (zero? i)
                              (swap! t-f not))
                            @t-f))
                     (range 10))]
    (is (not (every? identity ed)))
    (reset! t-f false)
    (is (safer/every? identity ed))))

(deftest not-every?-mutation-test
  (let [t-f (atom false)
        ed (eduction (map (fn [i]
                            (when (zero? i)
                              (swap! t-f not))
                            @t-f))
                     (range 10))]
    (is (not-every? identity ed))
    (reset! t-f false)
    (is (not (safer/not-every? identity ed)))))

(deftest drop-last-mutation-test
  (let [t-f (atom false)
        ed (eduction (filter (fn [i]
                               (when (zero? i)
                                 (swap! t-f not))
                               @t-f))
                     (range 10))]
    (is (= [] (drop-last ed)))
    (reset! t-f false)
    (is (= [0 1 2 3 4 5 6 7 8] (safer/drop-last ed)))
    (reset! t-f false)
    (is (= [] (drop-last 5 ed)))
    (reset! t-f false)
    (is (= [0 1 2 3 4] (safer/drop-last 5 ed)))))

(deftest take-last-mutation-test
  (let [t-f (atom false)
        ed (eduction (filter (fn [i]
                               (when (zero? i)
                                 (swap! t-f not))
                               @t-f))
                     (range 10))]
    (is (= [0] (take-last 5 ed)))
    (reset! t-f false)
    (is (= [5 6 7 8 9] (safer/take-last 5 ed)))))

(deftest sort-mutation-test
  (let [t-f (atom false)
        ed (eduction (filter (fn [i]
                               (when (zero? i)
                                 (swap! t-f not))
                               @t-f))
                     (range 10))]
    (is (thrown? NullPointerException (sort ed)))
    (reset! t-f false)
    (is (= [] (safer/sort ed)))))

(deftest sort-by-mutation-test
  (let [t-f (atom false)
        ed (eduction (filter (fn [i]
                               (when (zero? i)
                                 (swap! t-f not))
                               @t-f))
                     (range 10))]
    (is (thrown? NullPointerException (sort-by identity ed)))
    (reset! t-f false)
    (is (= [] (safer/sort-by identity ed)))))

(defn splitv-at
  "Returns a vector of [(into [] (take n) coll) (drop n coll)]"
  {:added "1.12"}
  [n coll]
  [(into [] (take n) coll) (drop n coll)])

(deftest splitv-at-mutation-test
  (let [up-down (atom true)
        ed (eduction (map (fn [i]
                            (when (zero? i)
                              (swap! up-down not))
                            (if @up-down
                              (- 9 i)
                              i)))
                     (range 10))]
    (is (= [[0 1 2 3 4] [4 3 2 1 0]] (splitv-at 5 ed)))
    (reset! up-down true)
    (is (= [[0 1 2 3 4] [5 6 7 8 9]] (safer/splitv-at 5 ed)))))
