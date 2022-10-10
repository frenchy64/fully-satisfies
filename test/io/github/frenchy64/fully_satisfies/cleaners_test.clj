(ns io.github.frenchy64.fully-satisfies.cleaners-test
  "Goal: use Java Cleaners to test for memory leaks"
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [testing deftest]]
            [io.github.frenchy64.fully-satisfies.cleaners :refer [register-cleaner! try-forcing-cleaners!
                                                                  head-hold-detecting-lazy-seq]]))

#_
(deftest try-forcing-cleaners!-test
  (let [cleaned? (atom [])
        _ (doto (volatile! (register-cleaner! (Object.) #(swap! cleaned? conj true)))
            (vreset! nil))]
    (try-forcing-cleaners!)))

(defn is-live [expected live]
  (try-forcing-cleaners! #(= expected @live))
  (is (= expected @live)))

(deftest reduce2-processes-sequentially-after-first-test
  (let [{:keys [live lseq]} (head-hold-detecting-lazy-seq)
        times (atom 0)]
    (reduce (fn [_ _]
              (let [t (swap! times inc)]
                (is-live (if (= 1 t)
                           #{0 1}
                           #{t})
                         live)))
            (take 10 lseq))
    (is-live #{} live)))

(deftest reduce3-processes-sequentially-test
  (let [{:keys [live lseq]} (head-hold-detecting-lazy-seq)]
    (reduce (fn [i _]
              (is-live #{i} live)
              (inc i))
            0 (take 10 lseq))
    (is-live #{} live)))

(deftest head-holding-test
  (let [{:keys [live lseq]} (head-hold-detecting-lazy-seq)
        head-holder (volatile! (doall (take 10 lseq)))]
    (is-live (into #{} (range 10))
             live)
    (vreset! head-holder nil)
    (is-live #{} live)))

(deftest map-does-not-chunk-lazy-seq-test
  (let [{:keys [live lseq]} (head-hold-detecting-lazy-seq)
        head-holder (volatile! (doto (map identity lseq)
                                 seq))]
    (is-live #{0} live)
    (vreset! head-holder nil)
    (is-live #{} live)))
