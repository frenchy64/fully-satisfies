(ns io.github.frenchy64.fully-satisfies.cleaners-test
  "Goal: use Java Cleaners to test for memory leaks"
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [testing deftest]]))

(defmacro ^:private when-jdk9 [& body]
  (when (try (Class/forName "java.lang.ref.Cleaner")
             (catch Throwable _))
    `(do ~@body)))

(when-jdk9
  (require '[io.github.frenchy64.fully-satisfies.cleaners :refer [register-cleaner! try-forcing-cleaners!
                                                                  head-hold-detecting-lazy-seq
                                                                  head-hold-detecting-chunked-seq
                                                                  is-live]]))

(when-jdk9
  (deftest try-forcing-cleaners!-test
    (let [cleaned? (atom [])
          _ (doto (volatile! (register-cleaner! (Object.) #(swap! cleaned? conj true)))
              (vreset! nil))]
      (try-forcing-cleaners!))))

(when-jdk9
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
      (is-live #{} live))))

(when-jdk9
  (deftest reduce3-processes-sequentially-test
    (let [{:keys [live lseq]} (head-hold-detecting-lazy-seq)]
      (reduce (fn [i _]
                (is-live #{i} live)
                (inc i))
              0 (take 10 lseq))
      (is-live #{} live))))

(when-jdk9
  (deftest head-holding-test
    (let [{:keys [live lseq]} (head-hold-detecting-lazy-seq)
          head-holder (volatile! (doall (take 10 lseq)))]
      (is-live (into #{} (range 10))
               live)
      (vreset! head-holder nil)
      (is-live #{} live))))

(when-jdk9
  (deftest map-does-not-chunk-lazy-seq-test
    (let [{:keys [live lseq]} (head-hold-detecting-lazy-seq)
          head-holder (volatile! (doto (map identity lseq)
                                   seq))]
      (is-live #{0} live)
      (vreset! head-holder nil)
      (is-live #{} live))))

(when-jdk9
  (deftest map-chunks-chunked-seq-test
    (let [{:keys [live lseq]} (head-hold-detecting-chunked-seq)
          head-holder (atom (map identity lseq))]
      (is-live #{} live)
      (dotimes [i 32]
        (prn i)
        (swap! head-holder next)
        (is-live (into (sorted-set) (range 32)) live))
      (reset! head-holder nil)
      (is-live #{} live))))

(defn synchronous-seque [buffer-size s]
  {:pre [(pos? buffer-size)]}
  (let [synchronous-seque (fn synchronous-seque [s]
                            (if-some [s (not-empty s)]
                              (do (nthnext s buffer-size)
                                  (lazy-seq
                                    (cons (first s) (lazy-seq (synchronous-seque (rest s))))))
                              (lazy-seq)))]
    (synchronous-seque (lazy-seq s))))

(deftest synchronous-seque-test
  (is (= [0 1 2 3 4 5] (seque 6 (range 6))))
  (is (= [0 1 2 3 4 5] (synchronous-seque 6 (range 6))))
  (let [a (atom [])
        s (seque 6 (repeatedly 40
                               (fn []
                                 (swap! a conj 0)
                                 0)))]
    ;(release-pending-sends)
    (is (= [0] (doall (take 1 s))))
    ; should be 7 not 9
    (is (= (repeat 9 0) @a)))
  (testing "synchronous-seque"
    (let [a (atom [])]
      (is (= [0]
             (doall
               (take 1 (synchronous-seque 6 (repeatedly 40
                                               (fn []
                                                 (swap! a conj 0)
                                                 0)))))))
      (is (= (repeat 7 0) @a)))))

(when-jdk9
  (deftest seque-look-ahead-test
    (doseq [seque [#'synchronous-seque
                   ;#'seque
                   ]]
      (testing (pr-str seque)
        (let [{:keys [live lseq]} (head-hold-detecting-lazy-seq)
              head-holder (volatile! (doto (seque 5 lseq)
                                       seq))]
          (first @head-holder)
          (is-live (into #{} (range 6)) live)
          (vreset! head-holder nil)
          (is-live #{} live))))))

(when-jdk9
  (deftest seque-reduce-look-ahead-test
    (doseq [seque [#'synchronous-seque
                   ;#'seque
                   ]]
      (testing (pr-str seque)
        (let [{:keys [live lseq]} (head-hold-detecting-lazy-seq)
              len 20
              buffer-size 5]
          (reduce (fn [i _]
                    (testing (pr-str i)
                      (is-live (into #{}
                                     (range i
                                            (min len (+ i buffer-size 1))))
                               live))
                    (inc i))
                  0
                  (seque buffer-size (take len lseq)))
          (is-live #{} live))))))

(when-jdk9
  (deftest seque-loop-look-ahead-test
    (doseq [seque [#'synchronous-seque
                   ;#'seque
                   ]]
      (testing (pr-str seque)
        (let [{:keys [live lseq]} (head-hold-detecting-lazy-seq)
              len 20
              buffer-size 5]
          (loop [i 0
                 c (seque buffer-size (take len lseq))]
            (when-some [c (seq c)]
              (testing (pr-str i)
                (is-live (into #{}
                               (range i
                                      (min len (+ i buffer-size 1))))
                         live))
              (recur (inc i) (next c))))
          (is-live #{} live))))))
