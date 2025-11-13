(ns io.github.frenchy64.fully-satisfies.linear-expansion-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core :as c]
            [io.github.frenchy64.fully-satisfies.linear-expansion :as fixed]))

(def ^:dynamic *counter* nil)
(defmacro counting-macro [] (swap! *counter* inc))

(defn is-count-expansions [expected form]
  (binding [*counter* (atom 0)]
    (eval form)
    (is (= expected @*counter*))))

(deftest counting-macro-test
  (is-count-expansions 0 `(do))
  (is-count-expansions 1 `(counting-macro)))

(deftest doseq-expands-exponentially-test
  (is-count-expansions 1 `(counting-macro))
  (is-count-expansions 1 `(c/doseq [] (counting-macro)))
  (is-count-expansions 2 `(c/doseq [_# nil] (counting-macro)))
  (is-count-expansions 16 `(c/doseq [_# nil _# nil _# nil _# nil] (counting-macro)))
  (is-count-expansions 8 `(c/doseq [_# nil _# nil _# nil _# (counting-macro)]))
  (is-count-expansions 15 `#(c/doseq [_# (counting-macro) _# (counting-macro) _# (counting-macro) _# (counting-macro)]))
  (is-count-expansions 31 `#(c/doseq [_# (counting-macro) _# (counting-macro) _# (counting-macro) _# (counting-macro)] (counting-macro)))
  (is-count-expansions 7 `#(c/doseq [_# (counting-macro) _# (counting-macro)] (counting-macro)))
  (is-count-expansions 5 `#(c/doseq [_# (counting-macro) :when (counting-macro)] (counting-macro)))
  (is-count-expansions 5 `#(c/doseq [_# (counting-macro) :while (counting-macro)] (counting-macro)))
  (is-count-expansions 5 `#(c/doseq [_# (counting-macro) :let [_# (counting-macro)]] (counting-macro))))

(deftest doseq-single-variable-test
  (is (macroexpand-1 `(doseq [~'v ~'e] ~'body))))

(deftest fixed-doseq-duplicates-no-expansion-test
  (is-count-expansions 1 `(counting-macro))
  (is-count-expansions 1 `(fixed/doseq [] (counting-macro)))
  (is-count-expansions 1 `(fixed/doseq [_# nil] (counting-macro)))
  (is-count-expansions 1 `(fixed/doseq [_# nil _# nil _# nil _# nil] (counting-macro)))
  (is-count-expansions 1 `(fixed/doseq [_# nil _# nil _# nil _# (counting-macro)]))
  (is-count-expansions 4 `#(fixed/doseq [_# (counting-macro) _# (counting-macro) _# (counting-macro) _# (counting-macro)]))
  (is-count-expansions 5 `#(fixed/doseq [_# (counting-macro) _# (counting-macro) _# (counting-macro) _# (counting-macro)] (counting-macro)))
  (is-count-expansions 3 `#(fixed/doseq [_# (counting-macro) _# (counting-macro)] (counting-macro)))
  (is-count-expansions 3 `#(fixed/doseq [_# (counting-macro) :when (counting-macro)] (counting-macro)))
  (is-count-expansions 3 `#(fixed/doseq [_# (counting-macro) :while (counting-macro)] (counting-macro)))
  (is-count-expansions 3 `#(fixed/doseq [_# (counting-macro) :let [_# (counting-macro)]] (counting-macro)))
)

(defn c-doseq-1 [c f] (c/doseq [i c] (f ::c/doseq i)))
(defn f-doseq-1 [c f] (fixed/doseq [i c] (f ::fixed/doseq i)))
(defn c-doseq-2 [c1 c2 f] (c/doseq [i c1 j c2] (f ::c/doseq [i j])))
(defn f-doseq-2 [c1 c2 f] (fixed/doseq [i c1 j c2] (f ::fixed/doseq [i j])))
(defn c-doseq-3 [c1 c2 c3 f] (c/doseq [i c1 j c2 k c3] (f ::c/doseq [i j k])))
(defn f-doseq-3 [c1 c2 c3 f] (fixed/doseq [i c1 j c2 k c3] (f ::fixed/doseq [i j k])))

(deftest fixed-doseq-parity-test
  (doseq [c1 [[0 1]
              (range 2)
              (sorted-set 0 1)]
          :let [_ (testing "1 variable"
                    (let [a (atom {})
                          f #(swap! a update %1 (fnil conj []) %2)
                          expected [0 1]]
                      (c-doseq-1 c1 f)
                      (f-doseq-1 c1 f)
                      (is (= {::c/doseq expected ::fixed/doseq expected} @a))))]
          c2 [[1 0]
              (range 1 -1 -1)
              (sorted-set-by (comp - compare) 0 1)]]
    (testing "2 variables"
      (let [a (atom {})
            f #(swap! a update %1 (fnil conj []) %2)
            expected [[0 1] [0 0] [1 1] [1 0]]]
        (c-doseq-2 c1 c2 f)
        (f-doseq-2 c1 c2 f)
        (is (= {::c/doseq expected ::fixed/doseq expected} @a))))
    (testing "3 variables"
      (let [a (atom {})
            c3 c2
            f #(swap! a update %1 (fnil conj []) %2)
            expected [[0 1 1] [0 1 0] [0 0 1] [0 0 0]
                      [1 1 1] [1 1 0] [1 0 1] [1 0 0]]]
        (c-doseq-3 c1 c2 c3 f)
        (f-doseq-3 c1 c2 c3 f)
        (is (= {::c/doseq expected ::fixed/doseq expected} @a)))))
  (let [total-iterations (atom {::c/doseq 0 ::fixed/doseq 0})
        expected-iterations 182250
        size 10]
    (doseq [i (range size)
            j (range size)
            k (range size)
            b1 [true false]
            b2 [true false]
            :let [c1 (range i)
                  c2 (range j)
                  c3 (range k)]]
      (assert (not (Thread/interrupted)))
      (let [a (atom {::c/doseq [] ::fixed/doseq []})
            f #(swap! a update %1 conj %2)
            _ (c/doseq [i c1 :when b1 j c2 :let [foo (if b2 c1 c2)] k c3]
                (swap! total-iterations update ::c/doseq inc)
                (f ::c/doseq [i j k foo]))
            _ (fixed/doseq [i c1 :when b1 j c2 :let [foo (if b2 c1 c2)] k c3]
                (swap! total-iterations update ::fixed/doseq inc)
                (f ::fixed/doseq [i j k foo]))
            {cdoseq ::c/doseq
             fdoseq ::fixed/doseq} @a]
        (assert (and cdoseq fdoseq))
        (is (= cdoseq fdoseq))))
    (is (= {::c/doseq expected-iterations
            ::fixed/doseq expected-iterations} @total-iterations))))

(deftest fixed-doseq-single-variable-test
  (is (macroexpand-1 `(fixed/doseq [~'v ~'e] ~'body)))
  )

(deftest for-expands-exponentially-test
  (is (thrown? Exception (eval `(c/for [] (counting-macro)))))
  (is-count-expansions 2 `(c/for [_# nil] (counting-macro)))
  (is-count-expansions 2 `(c/for [_# nil _# nil _# nil _# nil] (counting-macro)))
  (is-count-expansions 1 `(c/for [_# nil _# nil _# nil _# (counting-macro)] _#))
  (is-count-expansions 4 `(c/for [_# (counting-macro) _# (counting-macro) _# (counting-macro) _# (counting-macro)] _#))
  (is-count-expansions 16 `(c/for [_# nil] (c/for [_# nil] (c/for [_# nil] (c/for [_# nil] (counting-macro))))))
  (is-count-expansions 64 `(c/for [_# nil] (c/for [_# nil] (c/for [_# nil] (c/for [_# nil] (c/for [_# nil] (c/for [_# nil] (counting-macro))))))))
  (is-count-expansions 6 `(c/for [_# (counting-macro) _# (counting-macro) _# (counting-macro) _# (counting-macro)] (counting-macro))))

(comment
  (defmacro printing-macro []
    #_
    (try (throw (Exception.))
         (catch Exception e (prn e)))
    (prn "expanded")
    :printing-macro)
  (fixed/for [_# nil] (printing-macro))
  (do  (printing-macro))

(clojure.core/let
[iter__29499__auto__
 (clojure.core/fn
  iter__29550
  [to-process__29551]
  (clojure.core/lazy-seq
   (clojure.core/loop
    [to-process__29551
     to-process__29551
     chunk-index__29552
     (clojure.core/int 0)
     csize__29556
     (clojure.core/int 1)
     chunk__29555
     nil
     citer__29557
     true
     chunk-buffer__29553
     nil
     c?__29554
     false]
    (if
     (clojure.core/< chunk-index__29552 csize__29556)
     (clojure.core/when-let
      [to-process__29551
       (if
        c?__29554
        to-process__29551
        (clojure.core/seq to-process__29551))]
      (clojure.core/let
       [chunked__29497__auto__
        (clojure.core/chunked-seq? to-process__29551)
        newly-chunked?__29498__auto__
        (if c?__29554 false chunked__29497__auto__)
        chunk-index__29552
        (if
         newly-chunked?__29498__auto__
         (clojure.core/int 0)
         chunk-index__29552)
        chunk__29555
        (if
         newly-chunked?__29498__auto__
         (clojure.core/chunk-first to-process__29551)
         chunk__29555)
        csize__29556
        (if
         newly-chunked?__29498__auto__
         (clojure.core/int (clojure.core/count chunk__29555))
         csize__29556)
        chunk-buffer__29553
        (if
         newly-chunked?__29498__auto__
         (clojure.core/chunk-buffer csize__29556)
         chunk-buffer__29553)
        c?__29554
        (clojure.core/or c?__29554 chunked__29497__auto__)
        _#
        (if
         c?__29554
         (.nth chunk__29555 chunk-index__29552)
         (clojure.core/first to-process__29551))]
       (clojure.core/let
        [body__29549 (printing-macro)]
        (if
         c?__29554
         (do
          (clojure.core/chunk-append chunk-buffer__29553 body__29549)
          (recur
           to-process__29551
           (clojure.core/unchecked-inc chunk-index__29552)
           csize__29556
           chunk__29555
           citer__29557
           chunk-buffer__29553
           c?__29554))
         (clojure.core/cons
          body__29549
          (iter__29550 (clojure.core/rest to-process__29551)))))))
     (if
      citer__29557
      (clojure.core/chunk-cons
       (clojure.core/chunk chunk-buffer__29553)
       (iter__29550 (clojure.core/chunk-rest to-process__29551)))
      (clojure.core/chunk-cons
       (clojure.core/chunk chunk-buffer__29553)
       nil))))))]
(iter__29499__auto__ nil))

;; expands twice
(loop [a 1]
  (printing-macro)
  (when false
    (recur nil)))
  )

(deftest for-single-variable-test
  (is (macroexpand-1 `(fixed/for [~'v ~'e] ~'body))))

(deftest fixed-for-duplicates-no-expansion-test
  (is (thrown? Exception (eval `(fixed/for [] (counting-macro)))))
  (is-count-expansions 1 `(fixed/for [_# nil] (counting-macro)))
  (is-count-expansions 1 `(fixed/for [_# nil _# nil _# nil _# nil] (counting-macro)))
  (is-count-expansions 1 `(fixed/for [_# nil _# nil _# nil _# (counting-macro)] _#))
  (is-count-expansions 4 `(fixed/for [_# (counting-macro) _# (counting-macro) _# (counting-macro) _# (counting-macro)] _#))
  (is-count-expansions 1 `(fixed/for [_# nil] (fixed/for [_# nil] (fixed/for [_# nil] (fixed/for [_# nil] (counting-macro))))))
  (is-count-expansions 1 `(fixed/for [_# nil] (fixed/for [_# nil] (fixed/for [_# nil] (fixed/for [_# nil] (fixed/for [_# nil] (counting-macro)))))))
  (is-count-expansions 1 `(fixed/for [_# nil] (fixed/for [_# nil] (fixed/for [_# nil] (fixed/for [_# nil] (fixed/for [_# nil] (fixed/for [_# nil] (counting-macro))))))))
  (is-count-expansions 5 `(fixed/for [_# (counting-macro) _# (counting-macro) _# (counting-macro) _# (counting-macro)] (counting-macro))))

(deftest fixed-for-single-variable-test
  (is (macroexpand-1 `(fixed/for [~'v ~'e] ~'body))))

(deftest fixed-for-parity-test
  (doseq [c1 [[0 1]
              (range 2)
              (sorted-set 0 1)]
          :let [_ (testing "1 variable"
                    (is (= [0 1]
                           (c/for [i c1] i)
                           (fixed/for [i c1] i))))]
          c2 [[1 0]
              (range 1 -1 -1)
              (sorted-set-by (comp - compare) 0 1)]]
    (testing "2 variables"
      (is (= [[0 1] [0 0] [1 1] [1 0]]
             (c/for [i c1 j c2] [i j])
             (fixed/for [i c1 j c2] [i j]))))
    (testing "3 variables"
      (let [c3 c2]
        (is (= [[0 1 1] [0 1 0] [0 0 1] [0 0 0]
                [1 1 1] [1 1 0] [1 0 1] [1 0 0]]
               (c/for [i c1 j c2 k c3] [i j k])
               (fixed/for [i c1 j c2 k c3] [i j k]))))))
  (let [size 10]
    (doseq [i (range size)
            j (range size)
            k (range size)
            b1 [true false]
            b2 [true false]
            :let [c1 (range i)
                  c2 (range j)
                  c3 (range k)]]
      (assert (not (Thread/interrupted)))
      (let [core (c/for [i c1 :when b1 j c2 :let [foo (if b2 c1 c2)] k c3]
                   [i j k foo])
            fixed (fixed/for [i c1 :when b1 j c2 :let [foo (if b2 c1 c2)] k c3]
                    [i j k foo])]
        (is (= core fixed))))))

(deftest for-expansion-test
  (is (= (fixed/for [x (range 3) y (range 3) :let [z (+ x y)] :when (odd? z)] [x y z])
         '([0 1 1] [1 0 1] [1 2 3] [2 1 3]))))
