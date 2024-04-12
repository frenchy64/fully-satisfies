(ns io.github.frenchy64.fully-satisfies.lazier-test
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [testing deftest]]
            [io.github.frenchy64.fully-satisfies.lazier :as lazier]))

(defn lazy-range []
  (let [step (fn step [i]
               (lazy-seq
                 (cons i (step (inc i)))))]
    (step 0)))

(comment
  (do (lazier/dorun 0 (map prn (lazy-range)))
      nil)
  (do (dorun 1 (map prn (lazy-range)))
      nil)
  (do (lazier/dorun 1 (map prn (lazy-range)))
      nil)
  )

(deftest lazier-dorun-test
  (testing "dorun 0"
    (let [realized (atom #{})]
      (dorun 0 (map #(swap! realized conj %) (lazy-range)))
      (is (= #{0} @realized))))
  (testing "lazier/dorun 0"
    (let [realized (atom #{})]
      (lazier/dorun 0 (map #(swap! realized conj %) (lazy-range)))
      (is (= #{} @realized))))
  (testing "dorun 1"
    (let [realized (atom #{})]
      (dorun 1 (map #(swap! realized conj %) (lazy-range)))
      (is (= #{0 1} @realized))))
  (testing "lazier/dorun 1"
    (let [realized (atom #{})]
      (lazier/dorun 1 (map #(swap! realized conj %) (lazy-range)))
      (is (= #{0} @realized))))
  (testing "dorun 10"
    (let [realized (atom #{})]
      (dorun 10 (map #(swap! realized conj %) (lazy-range)))
      (is (= (into #{} (range 11)) @realized))))
  (testing "lazier/dorun 10"
    (let [realized (atom #{})]
      (lazier/dorun 10 (map #(swap! realized conj %) (lazy-range)))
      (is (= (into #{} (range 10)) @realized)))))

(deftest lazier-cycle-test
  (testing "cycle init"
    (let [realized (atom #{})]
      (cycle (map #(swap! realized conj %) (lazy-range)))
      (is (= #{0} @realized))))
  (testing "lazier/cycle init"
    (let [realized (atom #{})]
      (lazier/cycle (map #(swap! realized conj %) (lazy-range)))
      (is (= #{} @realized))))
  (testing "cycle next"
    (let [realized (atom #{})]
      (next (cycle (map #(swap! realized conj %) (lazy-range))))
      (is (= #{0} @realized))))
  (testing "lazier/cycle next"
    (let [realized (atom #{})]
      (next (lazier/cycle (map #(swap! realized conj %) (lazy-range))))
      (is (= #{0} @realized))))
  (testing "cycle nnext"
    (let [realized (atom #{})]
      (nnext (cycle (map #(swap! realized conj %) (lazy-range))))
      (is (= #{0 1} @realized))))
  (testing "lazier/cycle nnext"
    (let [realized (atom #{})]
      (nnext (lazier/cycle (map #(swap! realized conj %) (lazy-range))))
      (is (= #{0 1} @realized)))))

(deftest lazier-dedupe-test
  (testing "dedupe init"
    (let [realized (atom #{})]
      (dedupe (map #(swap! realized conj %) (lazy-range)))
      (is (= #{0} @realized))))
  (testing "lazier/dedupe init"
    (let [realized (atom #{})]
      (lazier/dedupe (map #(swap! realized conj %) (lazy-range)))
      (is (= #{} @realized))))
  (testing "dedupe seq"
    (let [realized (atom #{})]
      (seq (dedupe (map #(swap! realized conj %) (lazy-range))))
      (is (= (into (sorted-set) (range 33)) @realized))))
  (testing "lazier/dedupe seq"
    (let [realized (atom #{})]
      (seq (lazier/dedupe (map #(swap! realized conj %) (lazy-range))))
      (is (= (into (sorted-set) (range 32)) @realized)))))

(deftest lazier-bounded-count-test
  (testing "bounded-count 0"
    (let [realized (atom #{})]
      (is (= 0 (bounded-count 0 (map #(swap! realized conj %) (lazy-range)))))
      (is (= #{0} @realized))))
  (testing "lazier/bounded-count 0"
    (let [realized (atom #{})]
      (is (= 0 (lazier/bounded-count 0 (map #(swap! realized conj %) (lazy-range)))))
      (is (= #{} @realized))))
  (testing "bounded-count 10"
    (let [realized (atom #{})]
      (is (= 10 (bounded-count 10 (map #(swap! realized conj %) (lazy-range)))))
      (is (= (into (sorted-set) (range 11)) @realized))))
  (testing "lazier/bounded-count 10"
    (let [realized (atom #{})]
      (is (= 10 (lazier/bounded-count 10 (map #(swap! realized conj %) (lazy-range)))))
      (is (= (into (sorted-set) (range 10)) @realized)))))
