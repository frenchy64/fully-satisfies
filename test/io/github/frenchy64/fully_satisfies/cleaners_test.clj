(ns io.github.frenchy64.fully-satisfies.cleaners-test
  "Goal: use Java Cleaners to test for memory leaks"
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [testing deftest]]
            [io.github.frenchy64.fully-satisfies.lazier :as lazier]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reduce
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; take
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-jdk9
  (deftest head-holding-test
    (let [{:keys [live lseq]} (head-hold-detecting-lazy-seq)
          head-holder (volatile! (doall (take 10 lseq)))]
      (is-live (into #{} (range 10))
               live)
      (vreset! head-holder nil)
      (is-live #{} live))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-jdk9
  (deftest map-does-not-chunk-lazy-seq-test
    (let [{:keys [live lseq]} (head-hold-detecting-lazy-seq)
          head-holder (atom (map identity lseq))]
      (when (testing "initial call to map is lazy"
              (is-live #{} live))
        (when (every?
                (fn [i]
                  (swap! head-holder (if (zero? i) seq next))
                  (testing (str i " nexts holds 1 element")
                    (is-live #{i} live)))
                (range 32))
          (reset! head-holder nil)
          (testing "release hold"
            (is-live #{} live)))))))

(when-jdk9
  (deftest map-chunks-chunked-seq-test
    (let [{:keys [live lseq]} (head-hold-detecting-chunked-seq)
          head-holder (atom (map identity lseq))]
      (when (testing "initial call to map is lazy"
              (is-live #{} live))
        (when (every?
                (fn [i]
                  (swap! head-holder next)
                  (testing (str i " nexts holds 32 elements")
                    ;; map holds onto each chunk until the entire chunk
                    ;; is processed. this is because a chunk is an ArrayChunk
                    ;; which is backed by an array, and next just moves the start
                    ;; index forward. Since the array is shared between immutable
                    ;; seqs, it cannot be mutated.
                    (is-live (into (sorted-set) (range 32)) live)))
                (range 32))
          (reset! head-holder nil)
          (testing "release hold"
            (is-live #{} live)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-jdk9
  (deftest sequence-chunks-lazy-seq-test
    (let [{:keys [live lseq]} (head-hold-detecting-lazy-seq)
          head-holder (atom (sequence (map identity) lseq))]
      (when (testing "initial call to sequence realizes one element"
              ;;FIXME this is a bug. sequence doc says "will not force lazy seq"
              (is-live #{0} live))
        (when (testing "seq holds 33 elements"
                (swap! head-holder seq)
                ;;surprising
                (is-live (into (sorted-set) (range 33)) live))
          (when (every?
                  (fn [i]
                    (swap! head-holder next)
                    (testing (str i " nexts holds 33 elements")
                      (is-live (into (sorted-set) (range 33)) live)))
                  (range 32))
            (reset! head-holder nil)
            (testing "release hold"
              (is-live #{} live))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lazier/sequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-jdk9
  (deftest lazier-sequence-chunks-lazy-seq-test
    (let [{:keys [live lseq]} (head-hold-detecting-lazy-seq)
          head-holder (atom (lazier/sequence (map identity) lseq))]
      (when (testing "initial call to sequence realizes one element"
              (is-live #{} live))
        (when (testing "seq holds 32 elements"
                (swap! head-holder seq)
                (is-live (into (sorted-set) (range 32)) live))
          (when (every?
                  (fn [i]
                    (swap! head-holder next)
                    (testing (str i " nexts holds 32 elements")
                      (is-live (into (sorted-set) (range 32)) live)))
                  (range 31))
            (reset! head-holder nil)
            (testing "release hold"
              (is-live #{} live))))))))

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
