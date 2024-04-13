(ns io.github.frenchy64.fully-satisfies.safer-test
  (:refer-clojure :exclude [splitv-at partitionv-all])
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [io.github.frenchy64.fully-satisfies.safer :as safer]))

(deftest split-at-mutation-test
  (let [up-down (atom true)
        init! #(reset! up-down true)
        ed (eduction (map (fn [i]
                            (when (zero? i)
                              (swap! up-down not))
                            (if @up-down
                              (- 9 i)
                              i)))
                     (range 10))]
    (testing "eduction alternates"
      (init!)
      (is (= [[0 1 2 3 4 5 6 7 8 9]
              [9 8 7 6 5 4 3 2 1 0]
              [0 1 2 3 4 5 6 7 8 9]
              [9 8 7 6 5 4 3 2 1 0]]
             (repeatedly 4 #(vec ed)))))
    (init!)
    (is (= [[0 1 2 3 4] [4 3 2 1 0]] (split-at 5 ed)))
    (init!)
    (is (= [[0 1 2 3 4] [5 6 7 8 9]] (safer/split-at 5 ed)))))

(deftest split-with-mutation-test
  (let [t-f (atom false)
        init! #(reset! t-f false)
        ed (eduction (map (fn [i]
                            (when (zero? i)
                              (swap! t-f not))
                            @t-f))
                     (range 10))]
    (testing "eduction alternates"
      (init!)
      (is (= [[true true true true true true true true true true]
              [false false false false false false false false false false]
              [true true true true true true true true true true]
              [false false false false false false false false false false]]
             (repeatedly 4 #(vec ed)))))
    (init!)
    (is (= [[true true true true true true true true true true]
            [false false false false false false false false false false]]
           (split-with true? ed)))
    (init!)
    (is (= [[true true true true true true true true true true] []]
           (safer/split-with true? ed)))))

(deftest every?-mutation-test
  (let [t-f (atom false)
        init! #(reset! t-f false)
        ed (eduction (filter (fn [i]
                               (when (= 1 i)
                                 (swap! t-f not))
                               @t-f))
                     (range 1 11))]
    (testing "eduction alternates"
      (init!)
      (is (= [[1 2 3 4 5 6 7 8 9 10]
              []
              [1 2 3 4 5 6 7 8 9 10]
              []]
             (repeatedly 4 #(vec ed)))))
    (init!)
    (is (thrown? NullPointerException (every? pos? ed)))
    (init!)
    (is (safer/every? pos? ed))))

(deftest not-every?-mutation-test
  (let [t-f (atom false)
        init! #(reset! t-f false)
        ed (eduction (map (fn [i]
                            (when (zero? i)
                              (swap! t-f not))
                            @t-f))
                     (range 10))]
    (testing "eduction alternates"
      (init!)
      (is (= [[true true true true true true true true true true]
              [false false false false false false false false false false]
              [true true true true true true true true true true]
              [false false false false false false false false false false]]
             (repeatedly 4 #(vec ed)))))
    (init!)
    (is (not-every? identity ed))
    (init!)
    (is (not (safer/not-every? identity ed)))))

(deftest drop-last-mutation-test
  (let [t-f (atom false)
        init! #(reset! t-f false)
        ed (eduction (filter (fn [i]
                               (when (zero? i)
                                 (swap! t-f not))
                               @t-f))
                     (range 10))]
    (testing "eduction alternates"
      (init!)
      (is (= [[0 1 2 3 4 5 6 7 8 9]
              []
              [0 1 2 3 4 5 6 7 8 9]
              []]
             (repeatedly 4 #(vec ed)))))
    (init!)
    (is (= [] (drop-last ed)))
    (init!)
    (is (= [0 1 2 3 4 5 6 7 8] (safer/drop-last ed)))
    (init!)
    (is (= [] (drop-last 5 ed)))
    (init!)
    (is (= [0 1 2 3 4] (safer/drop-last 5 ed)))))

(deftest take-last-mutation-test
  (let [t-f (atom false)
        init! #(reset! t-f false)
        ed (eduction (filter (fn [i]
                               (when (zero? i)
                                 (swap! t-f not))
                               @t-f))
                     (range 10))]
    (testing "eduction alternates"
      (init!)
      (is (= [[0 1 2 3 4 5 6 7 8 9]
              []
              [0 1 2 3 4 5 6 7 8 9]
              []]
             (repeatedly 4 #(vec ed)))))
    (init!)
    (is (= [0] (take-last 5 ed)))
    (init!)
    (is (= [5 6 7 8 9] (safer/take-last 5 ed)))))

(deftest sort-mutation-test
  (let [t-f (atom false)
        init! #(reset! t-f false)
        ed (eduction (filter (fn [i]
                               (when (zero? i)
                                 (swap! t-f not))
                               @t-f))
                     (range 10))]
    (testing "eduction alternates"
      (init!)
      (is (= [[0 1 2 3 4 5 6 7 8 9]
              []
              [0 1 2 3 4 5 6 7 8 9]
              []]
             (repeatedly 4 #(vec ed)))))
    (init!)
    (is (thrown? NullPointerException (sort ed)))
    (init!)
    (is (= [] (safer/sort ed)))))

(deftest sort-by-mutation-test
  (let [t-f (atom false)
        init! #(reset! t-f false)
        ed (eduction (filter (fn [i]
                               (when (zero? i)
                                 (swap! t-f not))
                               @t-f))
                     (range 10))]
    (testing "eduction alternates"
      (init!)
      (is (= [[0 1 2 3 4 5 6 7 8 9]
              []
              [0 1 2 3 4 5 6 7 8 9]
              []]
             (repeatedly 4 #(vec ed)))))
    (init!)
    (is (thrown? NullPointerException (sort-by identity ed)))
    (init!)
    (is (= [] (safer/sort-by identity ed)))))

(defn splitv-at
  "Returns a vector of [(into [] (take n) coll) (drop n coll)]"
  {:added "1.12"}
  [n coll]
  [(into [] (take n) coll) (drop n coll)])

(deftest splitv-at-mutation-test
  (let [up-down (atom true)
        init! #(reset! up-down true)
        ed (eduction (map (fn [i]
                            (when (zero? i)
                              (swap! up-down not))
                            (if @up-down
                              (- 9 i)
                              i)))
                     (range 10))]
    (testing "eduction alternates"
      (init!)
      (is (= [[0 1 2 3 4 5 6 7 8 9]
              [9 8 7 6 5 4 3 2 1 0]
              [0 1 2 3 4 5 6 7 8 9]
              [9 8 7 6 5 4 3 2 1 0]]
             (repeatedly 4 #(vec ed)))))
    (init!)
    (is (= [[0 1 2 3 4] [4 3 2 1 0]] (splitv-at 5 ed)))
    (init!)
    (is (= [[0 1 2 3 4] [5 6 7 8 9]] (safer/splitv-at 5 ed)))))

(defn partitionv-all
  "Returns a lazy sequence of vector partitions, but may include
  partitions with fewer than n items at the end.
  Returns a stateful transducer when no collection is provided."
  {:added "1.12"}
  ([n]
   (partition-all n))
  ([n coll]
   (partitionv-all n n coll))
  ([n step coll]
   (lazy-seq
     (when-let [s (seq coll)]
       (let [seg (into [] (take n) coll)]
         (cons seg (partitionv-all n step (drop step s))))))))

(deftest partitionv-all-mutation-test
  (let [t-f (atom false)
        init! #(reset! t-f false)
        ed (eduction (filter (fn [i]
                               (when (zero? i)
                                 (swap! t-f not))
                               @t-f))
                     (range 10))]
    (testing "eduction alternates"
      (init!)
      (is (= [[0 1 2 3 4 5 6 7 8 9]
              []
              [0 1 2 3 4 5 6 7 8 9]
              []]
             (repeatedly 4 #(vec ed)))))
    (init!)
    (is (= [[]] (partitionv-all 5 ed)))
    (init!)
    (is (= [[0 1 2 3 4] [5 6 7 8 9]] (safer/partitionv-all 5 ed)))))

(deftest last-mutation-test
  (let [t-f (atom false)
        init! #(reset! t-f false)
        ed (eduction (filter (fn [i]
                               (when (zero? i)
                                 (swap! t-f not))
                               (if @t-f
                                 (< i 2)
                                 (< i 1))))
                     (range 3))]
    (testing "eduction alternates"
      (init!)
      (is (= [[0 1]
              [0]
              [0 1]
              [0]]
             (repeatedly 4 #(vec ed)))))
    (init!)
    (is (= nil (last ed)))
    (init!)
    (is (= 1 (safer/last ed)))))

(deftest butlast-mutation-test
  (let [t-f (atom false)
        init! #(reset! t-f false)
        ed (eduction (filter (fn [i]
                               (when (zero? i)
                                 (swap! t-f not))
                               (if @t-f
                                 (< i 2)
                                 (< i 0))))
                     (range 3))]
    (testing "eduction alternates"
      (init!)
      (is (= [[0 1]
              []
              [0 1]
              []]
             (repeatedly 4 #(vec ed)))))
    (init!)
    (is (= [nil] (butlast ed)))
    (init!)
    (is (= [0] (safer/butlast ed)))))
