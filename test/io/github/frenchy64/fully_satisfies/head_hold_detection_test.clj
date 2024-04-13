(ns io.github.frenchy64.fully-satisfies.head-hold-detection-test
  "Goal: use Java Cleaners to test for memory leaks"
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :as test-ctx :refer [testing]]
            [io.github.frenchy64.fully-satisfies.lazier :as lazier]
            [io.github.frenchy64.fully-satisfies.head-releasing :as head-releasing]))

(defmacro ^:private when-jdk9 [& body]
  (when (try (Class/forName "java.lang.ref.Cleaner")
             (catch Throwable _))
    `(do ~@body)))

(defmacro ^:private deftest [& args]
  `(when-jdk9
     (test-ctx/deftest ~@args)))

(when-jdk9
  (require '[io.github.frenchy64.fully-satisfies.cleaners :refer [register-cleaner! try-forcing-cleaners!
                                                                  head-hold-detecting-lazy-seq
                                                                  head-hold-detecting-chunked-seq
                                                                  is-strong]]))

(when-jdk9
  (deftest try-forcing-cleaners!-test
    (let [cleaned? (atom [])
          _ (doto (volatile! (register-cleaner! (Object.) #(swap! cleaned? conj true)))
              (vreset! nil))]
      (try-forcing-cleaners!))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest example-cleaners-test
  (let [{:keys [strong lseq]} (head-hold-detecting-lazy-seq
                              {:n 10}) ;; seq of fresh Object's, length 10
        ;; lseq=(...)
        _ (is-strong #{} strong) ;; no elements currently in memory
        lseq (seq lseq)
        ;; lseq=(0 ...)
        _ (is-strong #{0} strong) ;; just the first element in memory
        _ (nnext lseq)
        ;; lseq=(0 1 2...)
        _ (is-strong #{0 1 2} strong) ;; the first 3 elements in memory
        lseq (next lseq)
        ;; lseq=(1 2 ...)
        _ (is-strong #{1 2} strong) ;; the second 2 elements in memory
        lseq (rest lseq)
        ;; lseq=(2 ...)
        _ (is-strong #{2} strong) ;; the third element in memory
        lseq (rest lseq)
        ;; lseq=(...)
        _ (is-strong #{} strong) ;; no elements in memory
        ;; lseq=(3 ...)
        lseq (seq lseq)
        _ (is-strong #{3} strong) ;; fourth element in memory
        _ (class (first lseq)) ;; add a strong reference to lseq so previous line succeeds
        ;; lseq=nil
        _ (is-strong #{} strong) ;; lseq is entirely garbage collected
        ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reduce
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-jdk9
  (deftest reduce2-processes-sequentially-after-first-test
    (let [{:keys [strong lseq]} (head-hold-detecting-lazy-seq)
          times (atom 0)]
      (reduce (fn [_ _]
                (let [t (swap! times inc)]
                  (is-strong (if (= 1 t)
                             #{0 1}
                             #{t})
                           strong)))
              (take 10 lseq))
      (is-strong #{} strong))))

(when-jdk9
  (deftest reduce3-processes-sequentially-test
    (let [{:keys [strong lseq]} (head-hold-detecting-lazy-seq)]
      (reduce (fn [i _]
                (is-strong #{i} strong)
                (inc i))
              0 (take 10 lseq))
      (is-strong #{} strong))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; take
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-jdk9
  (deftest head-holding-test
    (let [{:keys [strong lseq]} (head-hold-detecting-lazy-seq)
          head-holder (volatile! (doall (take 10 lseq)))]
      (is-strong (into #{} (range 10))
               strong)
      (vreset! head-holder nil)
      (is-strong #{} strong))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; map / map-indexed / keep / keep-indexed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-jdk9
  (deftest map-does-not-chunk-lazy-seq-test
    (doseq [map [#'map #'map-indexed #'keep #'keep-indexed]]
      (testing map
        (let [{:keys [strong lseq]} (head-hold-detecting-lazy-seq)
              head-holder (atom (map vector lseq))]
          (when (testing "initial call to map is lazy"
                  (is-strong #{} strong))
            (when (every?
                    (fn [i]
                      (swap! head-holder (if (zero? i) seq next))
                      (testing (str i " nexts holds 1 element")
                        (is-strong #{i} strong)))
                    (range 32))
              (reset! head-holder nil)
              (testing "release hold"
                (is-strong #{} strong)))))))))

(when-jdk9
  (deftest map-chunks-chunked-seq-test
    (doseq [map [#'map #'map-indexed #'keep #'keep-indexed]]
      (testing map
        (let [{:keys [strong lseq]} (head-hold-detecting-chunked-seq)
              head-holder (atom (map vector lseq))]
          (when (testing "initial call to map is lazy"
                  (is-strong #{} strong))
            (when (every?
                    (fn [i]
                      (swap! head-holder next)
                      (testing (str i " nexts holds 32 elements")
                        ;; map holds onto each chunk until the entire chunk
                        ;; is processed. this is because a chunk is an ArrayChunk
                        ;; which is backed by an array, and next just moves the start
                        ;; index forward. Since the array is shared between immutable
                        ;; seqs, it cannot be mutated.
                        (is-strong (into (sorted-set) (range 32)) strong)))
                    (range 32))
              (reset! head-holder nil)
              (testing "release hold"
                (is-strong #{} strong)))))))))

(deftest map-head-holding-test
  (doseq [map [#'map #'map-indexed #'keep #'keep-indexed]]
    (testing map
      (let [{:keys [lseq strong]} (head-hold-detecting-lazy-seq)
            c (atom (map (constantly [nil]) lseq))
            _ (is-strong #{} strong)
            _ (swap! c seq)
            _ (is-strong #{} strong)
            _ (swap! c next)
            _ (is-strong #{} strong)
            ;; hold onto c
            _ (reset! c nil)
            _ (is-strong #{} strong)])
      (let [{:keys [lseq strong]} (head-hold-detecting-lazy-seq)
            c (atom (map (fn ([v] v) ([_ v] v)) lseq))
            ;; c=(...)
            _ (testing "init"
                (is-strong #{} strong))
            _ (swap! c seq)
            ;; c=(0 ...)
            _ (testing "seq"
                (is-strong #{0} strong))
            _ (next @c)
            ;; c=(0 1 ...)
            _ (testing "next!"
                (is-strong #{0 1} strong))
            _ (swap! c next)
            ;; c=(1 ...)
            _ (testing "swap next"
                (is-strong #{1} strong))
            _ (swap! c next)
            ;; c=(2 ...)
            _ (testing "swap nnext"
                (is-strong #{2} strong))
            _ (reset! c nil)
            ;; c=nil
            _ (is-strong #{} strong)]))))

(deftest map-head-holding-during-f-test
  (doseq [map [#'map #'map-indexed #'keep #'keep-indexed]]
    (testing map
      (let [{:keys [lseq strong]} (head-hold-detecting-lazy-seq)
            idx (atom -1)
            c (map (fn rec
                     ([_ v] (rec v))
                     ([v] (let [idx (swap! idx inc)
                                ;; not garbage collected because we have a strong
                                ;; reference below
                                _ (is-strong #{idx} strong)
                                _ (with-out-str (prn v))
                                ;; not garbage collected because map holds a strong
                                ;; reference to v because it calls rest after the
                                ;; fn returns.
                                _ (is-strong #{idx} strong)]
                            [nil])))
                   lseq)]
        (dorun 5 c)
        (is (= 5 @idx))))))

(deftest head-releasing-map-head-holding-during-f-test
  (doseq [map [#'head-releasing/map #'head-releasing/map-indexed
               #'head-releasing/keep #'head-releasing/keep-indexed]]
    (testing map
      (let [{:keys [lseq strong]} (head-hold-detecting-lazy-seq)
            idx (atom -1)
            c (map (fn rec
                     ([_ v] (rec v))
                     ([v]
                      (let [idx (swap! idx inc)
                            ;; not garbage collected because we have a strong
                            ;; reference below
                            _ (is-strong #{idx} strong)
                            _ (with-out-str (prn v))
                            ;; garbage collected!
                            _ (is-strong #{} strong)]
                        [nil])))
                   lseq)]
        (dorun 5 c)
        (is (= 5 @idx))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; every? / not-every?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest every?-head-holding-during-pred-test
  (doseq [every? [#'every? #'not-every?]]
    (testing every?
      (let [{:keys [lseq strong]} (head-hold-detecting-lazy-seq)
            idx (atom -1)]
        (every? (fn [v]
                  (let [idx (swap! idx inc)
                        ;; not garbage collected because we have a strong
                        ;; reference below
                        _ (is-strong #{idx} strong)
                        _ (with-out-str (prn v))
                        ;; not garbage collected because map holds a strong
                        ;; reference to v because it calls rest after the
                        ;; fn returns.
                        _ (is-strong #{idx} strong)]
                    (< idx 5)))
                lseq)
        (is (= 5 @idx))))))

(deftest head-releasing-every?-head-holding-during-pred-test
  (doseq [every? [#'head-releasing/every? #'head-releasing/not-every?]]
    (let [{:keys [lseq strong]} (head-hold-detecting-lazy-seq)
          idx (atom -1)]
      (every?
        (fn [v]
          (let [idx (swap! idx inc)
                ;; not garbage collected because we have a strong
                ;; reference below
                _ (is-strong #{idx} strong)
                _ (with-out-str (prn v))
                ;; garbage collected!
                _ (is-strong #{} strong)]
            (< idx 5)))
        lseq)
      (is (= 5 @idx)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some / not-any?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest some-head-holding-during-pred-test
  (doseq [some [#'some #'not-any?]]
    (testing some
      (let [{:keys [lseq strong]} (head-hold-detecting-lazy-seq)
            idx (atom -1)]
        (some (fn [v]
                (let [idx (swap! idx inc)
                      ;; not garbage collected because we have a strong
                      ;; reference below
                      _ (is-strong #{idx} strong)
                      _ (with-out-str (prn v))
                      ;; not garbage collected because map holds a strong
                      ;; reference to v because it calls rest after the
                      ;; fn returns.
                      _ (is-strong #{idx} strong)]
                  (when-not (< idx 5)
                    true)))
              lseq)
        (is (= 5 @idx))))))

(deftest head-releasing-some-head-holding-during-pred-test
  (doseq [some [#'head-releasing/some #'head-releasing/not-any?]]
    (testing some
      (let [{:keys [lseq strong]} (head-hold-detecting-lazy-seq)
            idx (atom -1)]
        (some
          (fn [v]
            (let [idx (swap! idx inc)
                  ;; not garbage collected because we have a strong
                  ;; reference below
                  _ (is-strong #{idx} strong)
                  _ (with-out-str (prn v))
                  ;; not garbage collected because map holds a strong
                  ;; reference to v because it calls rest after the
                  ;; fn returns.
                  _ (is-strong #{} strong)]
              (when-not (< idx 5)
                true)))
          lseq)
        (is (= 5 @idx))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; filter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest filter-head-holding-test
  (let [{:keys [lseq strong]} (head-hold-detecting-lazy-seq
                              {:n 100})
        c (atom (filter #(do % []) lseq))
        _ (is-strong #{} strong)
        _ (swap! c seq)
        _ (is-strong #{0} strong)
        _ (swap! c next)
        _ (is-strong #{1} strong)
        ;; hold onto c
        _ (reset! c nil)
        _ (is-strong #{} strong)])
  (let [{:keys [lseq strong]} (head-hold-detecting-lazy-seq
                              {:n 100})
        c (atom (filter #(do % []) lseq))
        ;; c=(...)
        _ (testing "init"
            (is-strong #{} strong))
        _ (swap! c seq)
        ;; c=(0 ...)
        _ (testing "seq"
            (is-strong #{0} strong))
        _ (next @c)
        ;; c=(0 1 ...)
        _ (testing "next!"
            (is-strong #{0 1} strong))
        _ (swap! c next)
        ;; c=(1 ...)
        _ (testing "swap next"
            (is-strong #{1} strong))
        _ (swap! c next)
        ;; c=(2 ...)
        _ (testing "swap nnext"
            (is-strong #{2} strong))
        _ (reset! c nil)
        ;; c=nil
        _ (is-strong #{} strong)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest remove-head-holding-test
  (let [{:keys [lseq strong]} (head-hold-detecting-lazy-seq
                              {:n 100})
        c (atom (remove #(do % nil) lseq))
        _ (is-strong #{} strong)
        _ (swap! c seq)
        _ (is-strong #{0} strong)
        _ (swap! c next)
        _ (is-strong #{1} strong)
        ;; hold onto c
        _ (reset! c nil)
        _ (is-strong #{} strong)])
  (let [{:keys [lseq strong]} (head-hold-detecting-lazy-seq
                              {:n 100})
        c (atom (remove #(do % nil) lseq))
        ;; c=(...)
        _ (testing "init"
            (is-strong #{} strong))
        _ (swap! c seq)
        ;; c=(0 ...)
        _ (testing "seq"
            (is-strong #{0} strong))
        _ (next @c)
        ;; c=(0 1 ...)
        _ (testing "next!"
            (is-strong #{0 1} strong))
        _ (swap! c next)
        ;; c=(1 ...)
        _ (testing "swap next"
            (is-strong #{1} strong))
        _ (swap! c next)
        ;; c=(2 ...)
        _ (testing "swap nnext"
            (is-strong #{2} strong))
        _ (reset! c nil)
        ;; c=nil
        _ (is-strong #{} strong)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; take-last
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest take-last-head-holding-test
  (let [{:keys [lseq strong]} (head-hold-detecting-lazy-seq
                              {:n 10})
        c (atom (take-last 6 lseq))
        _ (is-strong (into (sorted-set) (range 4 10)) strong)
        _ (swap! c seq)
        _ (is-strong (into (sorted-set) (range 4 10)) strong)
        _ (swap! c next)
        _ (is-strong (into (sorted-set) (range 5 10)) strong)
        ;; hold onto c
        _ (reset! c nil)
        _ (is-strong #{} strong)])
  (let [{:keys [lseq strong]} (head-hold-detecting-lazy-seq
                              {:n 10})
        c (atom (take-last 6 lseq))
        ;; c=(...)
        _ (testing "init"
            (is-strong (into (sorted-set) (range 4 10)) strong))
        _ (swap! c seq)
        ;; c=(0 1 2 3 4 5 6)
        _ (testing "seq"
            (is-strong (into (sorted-set) (range 4 10)) strong))
        _ (next @c)
        ;; c=(0 1 2 3 4 5 6 7)
        _ (testing "next!"
            (is-strong (into (sorted-set) (range 4 10)) strong))
        _ (swap! c next)
        ;; c=(1 2 3 4 5 6 7)
        _ (testing "swap next"
            (is-strong (into (sorted-set) (range 5 10)) strong))
        _ (swap! c next)
        ;; c=(2 3 4 5 6 7 8)
        _ (testing "swap nnext"
            (is-strong (into (sorted-set) (range 6 10)) strong))
        _ (reset! c nil)
        ;; c=nil
        _ (is-strong #{} strong)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; drop-last
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest drop-last-head-holding-test
  (let [{:keys [lseq strong]} (head-hold-detecting-lazy-seq
                              {:n 10})
        c (atom (drop-last 6 lseq))
        _ (is-strong #{} strong)
        _ (swap! c seq)
        _ (is-strong (into (sorted-set) (range 7)) strong)
        _ (swap! c next)
        _ (is-strong (into (sorted-set) (range 1 8)) strong)
        ;; hold onto c
        _ (reset! c nil)
        _ (is-strong #{} strong)])
  (let [{:keys [lseq strong]} (head-hold-detecting-lazy-seq
                              {:n 10})
        c (atom (drop-last 6 lseq))
        ;; c=(...)
        _ (testing "init"
            (is-strong #{} strong))
        _ (swap! c seq)
        ;; c=(0 1 2 3 4 5 6)
        _ (testing "seq"
            (is-strong (into (sorted-set) (range 7)) strong))
        _ (next @c)
        ;; c=(0 1 2 3 4 5 6 7)
        _ (testing "next!"
            (is-strong (into (sorted-set) (range 8)) strong))
        _ (swap! c next)
        ;; c=(1 2 3 4 5 6 7)
        _ (testing "swap next"
            (is-strong (into (sorted-set) (range 1 8)) strong))
        _ (swap! c next)
        ;; c=(2 3 4 5 6 7 8)
        _ (testing "swap nnext"
            (is-strong (into (sorted-set) (range 2 9)) strong))
        _ (reset! c nil)
        ;; c=nil
        _ (is-strong #{} strong)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; drop-while TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reduce
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest reduce-head-holding-test
  (let [{:keys [lseq strong]} (head-hold-detecting-lazy-seq
                              {:n 20
                               :i->v (fn [_] (volatile! (Object.)))})]
    (reduce (fn [i vol]
              (is-strong #{i} strong)
              (vreset! vol nil)
              ;; the Object impl of internal-reduce holds onto the
              ;; head of the seq while calling the reducing function.
              ;; this is because we need to wait to see if we return
              ;; reduced before deciding whether to call `next`. perhaps
              ;; we should call `rest` before calling the function?
              (is-strong #{i} strong)
              (inc i))
            0 lseq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; naive-seq-reduce
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest naive-seq-reduce-head-holding-test
  (let [{:keys [lseq strong]} (head-hold-detecting-lazy-seq
                              {:n 20
                               :i->v (fn [_] (volatile! (Object.)))})]
    (head-releasing/naive-seq-reduce
      lseq
      (fn [i vol]
        (is-strong #{i} strong)
        (vreset! vol nil)
        (is-strong #{} strong)
        (inc i))
      0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-jdk9
  (deftest sequence-chunks-lazy-seq-test
    (let [{:keys [strong lseq]} (head-hold-detecting-lazy-seq)
          head-holder (atom (sequence (map identity) lseq))]
      (when (testing "initial call to sequence realizes one element"
              ;;FIXME this is a bug. sequence doc says "will not force lazy seq"
              ;; https://clojure.atlassian.net/browse/CLJ-2795
              (is-strong #{0} strong))
        (when (testing "seq holds 33 elements"
                (swap! head-holder seq)
                ;;FIXME this is a bug. recursive call to chunkIteratorSeq calls
                ;; iter.hasNext () an extra time without adding to a chunk
                (is-strong (into (sorted-set) (range 33)) strong))
          (when (every?
                  (fn [i]
                    (swap! head-holder next)
                    (testing (str i " nexts holds 33 elements")
                      (is-strong (into (sorted-set) (range 33)) strong)))
                  (range 31))
            (reset! head-holder nil)
            (testing "release hold"
              (is-strong #{} strong))))))))

(comment
  (do (sequence (map #(prn "computed" %))
                (map #(do (prn "realized" %) %) (range)))
      nil)
  (do (nth (sequence (map #(prn "computed" %))
                     (map #(do (prn "realized" %) %) (range)))
           31)
      nil)
  (do (first (sequence (map #(prn "computed" %))
                       (map #(do (prn "realized" %) %) (range))))
      nil)
  (do (lazier/sequence (map #(prn "computed" %))
                       (map #(do (prn "realized" %) %) (range)))
      nil)
  (do (first (let [c (lazier/sequence (map #(prn "computed" %))
                                      (map #(do (prn "realized" %) %) (range)))]
               (prn "done")
               c))
      nil)
  (do (nth (lazier/sequence (map identity)
                            (map prn (range)))
           32)
      nil)
  (do (nth (lazier/sequence (map identity)
                               (map prn (range)))
           64)
      nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lazier/sequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-jdk9
  (deftest lazier-sequence-chunks-lazy-seq-test
    (let [{:keys [strong lseq]} (head-hold-detecting-lazy-seq)
          head-holder (atom (lazier/sequence (map identity) lseq))]
      (when (testing "initial call to sequence realizes no elements"
              (is-strong #{} strong))
        (when (testing "seq holds 32 elements"
                (swap! head-holder seq)
                (is-strong (into (sorted-set) (range 32)) strong))
          (when (every?
                  (fn [i]
                    (swap! head-holder next)
                    (testing (str i " nexts holds 32 elements")
                      (is-strong (into (sorted-set) (range 32)) strong)))
                  (range 31))
            (reset! head-holder nil)
            (testing "release hold"
              (is-strong #{} strong))))))))

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
        (let [{:keys [strong lseq]} (head-hold-detecting-lazy-seq)
              head-holder (volatile! (doto (seque 5 lseq)
                                       seq))]
          (first @head-holder)
          (is-strong (into #{} (range 6)) strong)
          (vreset! head-holder nil)
          (is-strong #{} strong))))))

(when-jdk9
  (deftest seque-reduce-look-ahead-test
    (doseq [seque [#'synchronous-seque
                   ;#'seque
                   ]]
      (testing (pr-str seque)
        (let [{:keys [strong lseq]} (head-hold-detecting-lazy-seq)
              len 20
              buffer-size 5]
          (reduce (fn [i _]
                    (testing (pr-str i)
                      (is-strong (into #{}
                                     (range i
                                            (min len (+ i buffer-size 1))))
                               strong))
                    (inc i))
                  0
                  (seque buffer-size (take len lseq)))
          (is-strong #{} strong))))))

(when-jdk9
  (deftest seque-loop-look-ahead-test
    (doseq [seque [#'synchronous-seque
                   ;#'seque
                   ]]
      (testing (pr-str seque)
        (let [{:keys [strong lseq]} (head-hold-detecting-lazy-seq)
              len 20
              buffer-size 5]
          (loop [i 0
                 c (seque buffer-size (take len lseq))]
            (when-some [c (seq c)]
              (testing (pr-str i)
                (is-strong (into #{}
                                 (range i
                                        (min len (+ i buffer-size 1))))
                           strong))
              (recur (inc i) (next c))))
          (is-strong #{} strong))))))
