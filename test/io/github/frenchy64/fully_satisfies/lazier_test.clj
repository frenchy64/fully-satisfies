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
