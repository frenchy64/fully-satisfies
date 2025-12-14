(ns io.github.frenchy64.fully-satisfies.linear-expansion-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core :as c]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [io.github.frenchy64.fully-satisfies.linear-expansion :as fixed]))

;; important: Auto-boxing warnings mean the Clojure compiler analyzes the body of a loop twice.
;; e.g., counting-macro is expanded twice here: (loop [a 1] (counting-macro) (recur nil))
;; must fix such warnings for reliable is-count-expansions calls.
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:dynamic *counter* nil)
(defmacro counting-macro [] (swap! *counter* inc) nil)

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

(defn pretty-doseq-expansion [form]
  (let [gensym-remap {"seq_" 's
                      "count_" 'count-
                      "chunk_" 'chunk-
                      "in-chunk_" 'in-chunk
                      "chunked?__" 'chunked?
                      "c_" 'f
                      "i_" 'i}
        banned-syms (set (vals gensym-remap))]
    (walk/postwalk (fn [form]
                     (cond
                       (and (qualified-symbol? form)
                            (= "clojure.core" (namespace form)))
                       (symbol (name form))

                       (simple-symbol? form)
                       (do (assert (not (banned-syms form)))
                           (or (some (fn [[prefix rewrite]]
                                       (when (str/starts-with? (name form) prefix)
                                         rewrite))
                                     gensym-remap)
                               form))

                       :else form))
                   form)))

(defn count-subforms
  "Approximate measure of expansion size"
  [form]
  (let [n (atom 0)]
    (walk/postwalk (fn [form]
                     (swap! n inc')
                     form)
                   form)
    @n))

(deftest fixed-doseq-duplicates-no-expansion-test
  (is-count-expansions 1 `(counting-macro))
  (is-count-expansions 1 `(fixed/doseq [] (counting-macro)))
  (is-count-expansions 1 `(fixed/doseq [_# nil] (counting-macro)))
  (is-count-expansions 1 `(fixed/doseq [_# (counting-macro)]))
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

(deftest doseq-expansion-comparison-test
  (testing '(doseq [] B)
    (is (= (pretty-doseq-expansion (macroexpand-1 `(doseq [] ~'B)))
           (pretty-doseq-expansion (macroexpand-1 `(fixed/doseq [] ~'B)))
           '(do B))))
  (testing '(doseq [V E] B)
    (testing 'c/doseq
      (is (= 89 (count-subforms (macroexpand-1 `(doseq [~'V ~'E] ~'B)))))
      (is (= (pretty-doseq-expansion (macroexpand-1 `(doseq [~'V ~'E] ~'B)))
             '(loop [s (seq E) chunk- nil count- 0 i 0]
                (if (< i count-)
                  (let [V (.nth chunk- i)]
                    (do B)
                    (recur s chunk- count- (unchecked-inc i)))
                  (when-let [s (seq s)]
                    (if (chunked-seq? s)
                      (let [f (chunk-first s)]
                        (recur (chunk-rest s) f (int (count f)) (int 0)))
                      (let [V (first s)]
                        (do B)
                        (recur (next s) nil 0 0)))))))))
    (testing 'fixed/doseq
      (is (= 100 (count-subforms (macroexpand-1 `(fixed/doseq [~'V ~'E] ~'B)))))
      (is (= (pretty-doseq-expansion (macroexpand-1 `(fixed/doseq [~'V ~'E] ~'B)))
             '(loop [s E chunk- nil count- 0 i 0]
                (let [in-chunk (< i count-)
                      s (if in-chunk s (seq s))]
                  (when (if in-chunk true s)
                    (if (if in-chunk false (chunked-seq? s))
                      (let [f (chunk-first s)]
                        (recur (chunk-rest s) f (int (count f)) (int 0)))
                      (let [V (if in-chunk (.nth chunk- i) (first s))]
                        (do B)
                        (if in-chunk
                          (recur s chunk- count- (unchecked-inc i))
                          (recur (next s) nil 0 0)))))))))))
  (testing '(doseq [:when P] B)
    (is (= (pretty-doseq-expansion (macroexpand-1 `(doseq [:when ~'P] ~'B)))
           (pretty-doseq-expansion (macroexpand-1 `(fixed/doseq [:when ~'P] ~'B)))
           '(if P (do (do B) nil) nil))))
  (testing '(doseq [V E :when P] B)
    (testing 'c/doseq
      (is (= 115 (count-subforms (macroexpand-1 `(doseq [~'V ~'E :when ~'P] ~'B)))))
      (is (= (pretty-doseq-expansion (macroexpand-1 `(doseq [~'V ~'E :when ~'P] ~'B)))
             '(loop [s (seq E) chunk- nil count- 0 i 0]
                (if (< i count-)
                  (let [V (.nth chunk- i)]
                    (if P
                      (do (do B)
                          (recur s chunk- count- (unchecked-inc i)))
                      (recur s chunk- count- (unchecked-inc i))))
                  (when-let [s (seq s)]
                    (if (chunked-seq? s)
                      (let [f (chunk-first s)]
                        (recur (chunk-rest s) f (int (count f)) (int 0)))
                      (let [V (first s)]
                        (if P
                          (do (do B)
                              (recur (next s) nil 0 0))
                          (recur (next s) nil 0 0))))))))))
    (testing 'fixed/doseq
      (is (= 124 (count-subforms (macroexpand-1 `(fixed/doseq [~'V ~'E :when ~'P] ~'B)))))
      (is (= (pretty-doseq-expansion (macroexpand-1 `(fixed/doseq [~'V ~'E :when ~'P] ~'B)))
             '(loop [s E chunk- nil count- 0 i 0]
                (let [in-chunk (< i count-) s (if in-chunk s (seq s))]
                  (when (if in-chunk true s)
                    (if (if in-chunk false (chunked-seq? s))
                      (let [f (chunk-first s)]
                        (recur (chunk-rest s) f (int (count f)) (int 0)))
                      (let [V (if in-chunk (.nth chunk- i) (first s))]
                        (if P
                          (do (do B)
                              (if in-chunk
                                (recur s chunk- count- (unchecked-inc i))
                                (recur (next s) nil 0 0)))
                          (if in-chunk
                            (recur s chunk- count- (unchecked-inc i))
                            (recur (next s) nil 0 0))))))))))))

  (testing '(doseq [:while P] B)
    (is (= (pretty-doseq-expansion (macroexpand-1 `(doseq [:while ~'P] ~'B)))
           (pretty-doseq-expansion (macroexpand-1 `(fixed/doseq [:while ~'P] ~'B)))
           '(when P (do B) nil))))
  (testing '(doseq [V E :while P] B)
    (testing 'c/doseq
      (is (= 95 (count-subforms (macroexpand-1 `(doseq [~'V ~'E :while ~'P] ~'B)))))
      (is (= (pretty-doseq-expansion (macroexpand-1 `(doseq [~'V ~'E :while ~'P] ~'B)))
             '(loop [s (seq E) chunk- nil count- 0 i 0]
                (if (< i count-)
                  (let [V (.nth chunk- i)]
                    (when P
                      (do B)
                      (recur s chunk- count- (unchecked-inc i))))
                  (when-let [s (seq s)]
                    (if (chunked-seq? s)
                      (let [f (chunk-first s)]
                        (recur (chunk-rest s) f (int (count f)) (int 0)))
                      (let [V (first s)]
                        (when P
                          (do B)
                          (recur (next s) nil 0 0))))))))))
    (testing 'fixed/doseq
      (is (= 103 (count-subforms (macroexpand-1 `(fixed/doseq [~'V ~'E :while ~'P] ~'B)))))
      (is (= (pretty-doseq-expansion (macroexpand-1 `(fixed/doseq [~'V ~'E :while ~'P] ~'B)))
             '(loop [s E chunk- nil count- 0 i 0]
                (let [in-chunk (< i count-) s (if in-chunk s (seq s))]
                  (when (if in-chunk true s)
                    (if (if in-chunk false (chunked-seq? s))
                      (let [f (chunk-first s)]
                        (recur (chunk-rest s) f (int (count f)) (int 0)))
                      (let [V (if in-chunk (.nth chunk- i) (first s))]
                        (when P
                          (do B)
                          (if in-chunk
                            (recur s chunk- count- (unchecked-inc i))
                            (recur (next s) nil 0 0))))))))))))
  (testing '(doseq [:let ['S 'T]] B)
    (is (= (pretty-doseq-expansion (macroexpand-1 `(fixed/doseq [:let [~'S ~'T]] ~'B)))
           (pretty-doseq-expansion (macroexpand-1 `(doseq [:let [~'S ~'T]] ~'B)))
           '(let [S T] (do B)))))
  (testing '(doseq [V0 E0 V1 E1] B)
    (testing 'c/doseq
      (is (= 261 (count-subforms (macroexpand-1 `(doseq ~'[V0 E0 V1 E1] ~'B)))))
      (is (= (pretty-doseq-expansion (macroexpand-1 `(doseq ~'[V0 E0 V1 E1] ~'B)))
             '(loop [s (seq E0) chunk- nil count- 0 i 0]
                (if (< i count-)
                  (let [V0 (.nth chunk- i)]
                    (loop [s (seq E1) chunk- nil count- 0 i 0]
                      (if (< i count-)
                        (let [V1 (.nth chunk- i)]
                          (do B)
                          (recur s chunk- count- (unchecked-inc i)))
                        (when-let [s (seq s)]
                          (if (chunked-seq? s)
                            (let [f (chunk-first s)]
                              (recur (chunk-rest s) f (int (count f)) (int 0)))
                            (let [V1 (first s)]
                              (do B)
                              (recur (next s) nil 0 0))))))
                    (recur s chunk- count- (unchecked-inc i)))
                  (when-let [s (seq s)]
                    (if (chunked-seq? s)
                      (let [f (chunk-first s)]
                        (recur (chunk-rest s) f (int (count f)) (int 0)))
                      (let [V0 (first s)]
                        (loop [s (seq E1) chunk- nil count- 0 i 0]
                          (if (< i count-)
                            (let [V1 (.nth chunk- i)]
                              (do B)
                              (recur s chunk- count- (unchecked-inc i)))
                            (when-let [s (seq s)]
                              (if (chunked-seq? s)
                                (let [f (chunk-first s)]
                                  (recur (chunk-rest s) f (int (count f)) (int 0)))
                                (let [V1 (first s)]
                                  (do B)
                                  (recur (next s) nil 0 0))))))
                        (recur (next s) nil 0 0)))))))))
    (testing 'fixed/doseq
      (is (= 197 (count-subforms (macroexpand-1 `(fixed/doseq ~'[V0 E0 V1 E1] ~'B)))))
      (is (= (pretty-doseq-expansion (macroexpand-1 `(fixed/doseq ~'[V0 E0 V1 E1] ~'B)))
             '(loop [s E0 chunk- nil count- 0 i 0]
                (let [in-chunk (< i count-) s (if in-chunk s (seq s))]
                  (when (if in-chunk true s)
                    (if (if in-chunk false (chunked-seq? s))
                      (let [f (chunk-first s)]
                        (recur (chunk-rest s) f (int (count f)) (int 0)))
                      (let [V0 (if in-chunk (.nth chunk- i) (first s))]
                        (loop [s E1 chunk- nil count- 0 i 0]
                          (let [in-chunk (< i count-) s (if in-chunk s (seq s))]
                            (when (if in-chunk true s)
                              (if (if in-chunk false (chunked-seq? s))
                                (let [f (chunk-first s)]
                                  (recur (chunk-rest s) f (int (count f)) (int 0)))
                                (let [V1 (if in-chunk (.nth chunk- i) (first s))]
                                  (do B)
                                  (if in-chunk
                                    (recur s chunk- count- (unchecked-inc i))
                                    (recur (next s) nil 0 0)))))))
                        (if in-chunk
                          (recur s chunk- count- (unchecked-inc i))
                          (recur (next s) nil 0 0)))))))))))
  (testing '(doseq [V0 E0 V1 E1 V2 E2] B)
    (testing 'c/doseq
      (is (= 605 (count-subforms (macroexpand-1 `(doseq ~'[V0 E0 V1 E1 V2 E2] ~'B)))))
      (is (= (pretty-doseq-expansion (macroexpand-1 `(doseq ~'[V0 E0 V1 E1 V2 E2] ~'B)))
             '(loop [s (seq E0) chunk- nil count- 0 i 0]
                (if (< i count-)
                  (let [V0 (.nth chunk- i)]
                    (loop [s (seq E1) chunk- nil count- 0 i 0]
                      (if (< i count-)
                        (let [V1 (.nth chunk- i)]
                          (loop [s (seq E2) chunk- nil count- 0 i 0]
                            (if (< i count-)
                              (let [V2 (.nth chunk- i)]
                                (do B)
                                (recur s chunk- count- (unchecked-inc i)))
                              (when-let [s (seq s)]
                                (if (chunked-seq? s)
                                  (let [f (chunk-first s)]
                                    (recur (chunk-rest s) f (int (count f)) (int 0)))
                                  (let [V2 (first s)]
                                    (do B)
                                    (recur (next s) nil 0 0))))))
                          (recur s chunk- count- (unchecked-inc i)))
                        (when-let [s (seq s)]
                          (if (chunked-seq? s)
                            (let [f (chunk-first s)]
                              (recur (chunk-rest s) f (int (count f)) (int 0)))
                            (let [V1 (first s)]
                              (loop [s (seq E2) chunk- nil count- 0 i 0]
                                (if (< i count-)
                                  (let [V2 (.nth chunk- i)]
                                    (do B)
                                    (recur s chunk- count- (unchecked-inc i)))
                                  (when-let [s (seq s)]
                                    (if (chunked-seq? s)
                                      (let [f (chunk-first s)]
                                        (recur (chunk-rest s) f (int (count f)) (int 0)))
                                      (let [V2 (first s)]
                                        (do B)
                                        (recur (next s) nil 0 0))))))
                              (recur (next s) nil 0 0))))))
                    (recur s chunk- count- (unchecked-inc i)))
                  (when-let [s (seq s)]
                    (if (chunked-seq? s)
                      (let [f (chunk-first s)]
                        (recur (chunk-rest s) f (int (count f)) (int 0)))
                      (let [V0 (first s)]
                        (loop [s (seq E1) chunk- nil count- 0 i 0]
                          (if (< i count-)
                            (let [V1 (.nth chunk- i)]
                              (loop [s (seq E2) chunk- nil count- 0 i 0]
                                (if (< i count-)
                                  (let [V2 (.nth chunk- i)]
                                    (do B)
                                    (recur s chunk- count- (unchecked-inc i)))
                                  (when-let [s (seq s)]
                                    (if (chunked-seq? s)
                                      (let [f (chunk-first s)]
                                        (recur (chunk-rest s) f (int (count f)) (int 0)))
                                      (let [V2 (first s)]
                                        (do B)
                                        (recur (next s) nil 0 0))))))
                              (recur s chunk- count- (unchecked-inc i)))
                            (when-let [s (seq s)]
                              (if (chunked-seq? s)
                                (let [f (chunk-first s)]
                                  (recur (chunk-rest s) f (int (count f)) (int 0)))
                                (let [V1 (first s)]
                                  (loop [s (seq E2) chunk- nil count- 0 i 0]
                                    (if (< i count-)
                                      (let [V2 (.nth chunk- i)]
                                        (do B)
                                        (recur s chunk- count- (unchecked-inc i)))
                                      (when-let [s (seq s)]
                                        (if (chunked-seq? s)
                                          (let [f (chunk-first s)]
                                            (recur (chunk-rest s) f (int (count f)) (int 0)))
                                          (let [V2 (first s)]
                                            (do B)
                                            (recur (next s) nil 0 0))))))
                                  (recur (next s) nil 0 0))))))
                        (recur (next s) nil 0 0)))))))))
    (testing 'fixed/doseq
      (is (= 294 (count-subforms (macroexpand-1 `(fixed/doseq ~'[V0 E0 V1 E1 V2 E2] ~'B)))))
      (is (= (pretty-doseq-expansion (macroexpand-1 `(fixed/doseq ~'[V0 E0 V1 E1 V2 E2] ~'B)))
             '(loop [s E0 chunk- nil count- 0 i 0]
                (let [in-chunk (< i count-)
                      s (if in-chunk s (seq s))]
                  (when (if in-chunk true s)
                    (if (if in-chunk false (chunked-seq? s))
                      (let [f (chunk-first s)]
                        (recur (chunk-rest s) f (int (count f)) (int 0)))
                      (let [V0 (if in-chunk (.nth chunk- i) (first s))]
                        (loop [s E1 chunk- nil count- 0 i 0]
                          (let [in-chunk (< i count-) s (if in-chunk s (seq s))]
                            (when (if in-chunk true s)
                              (if (if in-chunk false (chunked-seq? s))
                                (let [f (chunk-first s)]
                                  (recur (chunk-rest s) f (int (count f)) (int 0)))
                                (let [V1 (if in-chunk (.nth chunk- i) (first s))]
                                  (loop [s E2 chunk- nil count- 0 i 0]
                                    (let [in-chunk (< i count-) s (if in-chunk s (seq s))]
                                      (when (if in-chunk true s)
                                        (if (if in-chunk false (chunked-seq? s))
                                          (let [f (chunk-first s)]
                                            (recur (chunk-rest s) f (int (count f)) (int 0)))
                                          (let [V2 (if in-chunk (.nth chunk- i) (first s))]
                                            (do B)
                                            (if in-chunk
                                              (recur s chunk- count- (unchecked-inc i))
                                              (recur (next s) nil 0 0)))))))
                                  (if in-chunk
                                    (recur s chunk- count- (unchecked-inc i))
                                    (recur (next s) nil 0 0)))))))
                        (if in-chunk
                          (recur s chunk- count- (unchecked-inc i))
                          (recur (next s) nil 0 0))))))))))))

(deftest doseq-expansion-size-test
  (testing "(doseq [V0 E0 ... V8 E8] B)"
    (is (= (into (sorted-map-by (fn [l r] (compare (count-subforms l) (count-subforms r))))
                 (map (fn [ncolls]
                        (let [binder (into [] (comp (map (fn [i]
                                                           [(symbol (str "V" i))
                                                            (symbol (str "E" i))]))
                                                    cat)
                                           (range ncolls))
                              body 'B
                              ->form (fn [op] (list op binder body))
                              ->count (fn [op] (count-subforms (macroexpand-1 (->form op))))]
                          {(->form 'doseq)
                           (into {} (map (fn [op] {op (->count op)}))
                                 `#{c/doseq fixed/doseq})})))
                 (range 10))
           '{(doseq [] B)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 3,   clojure.core/doseq 3},
             (doseq [V0 E0] B)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 100, clojure.core/doseq 89},
             (doseq [V0 E0 V1 E1] B)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 197, clojure.core/doseq 261},
             (doseq [V0 E0 V1 E1 V2 E2] B)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 294, clojure.core/doseq 605},
             (doseq [V0 E0 V1 E1 V2 E2 V3 E3] B)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 391, clojure.core/doseq 1293},
             (doseq [V0 E0 V1 E1 V2 E2 V3 E3 V4 E4] B)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 488, clojure.core/doseq 2669},
             (doseq [V0 E0 V1 E1 V2 E2 V3 E3 V4 E4 V5 E5] B)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 585, clojure.core/doseq 5421},
             (doseq [V0 E0 V1 E1 V2 E2 V3 E3 V4 E4 V5 E5 V6 E6] B)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 682, clojure.core/doseq 10925},
             (doseq [V0 E0 V1 E1 V2 E2 V3 E3 V4 E4 V5 E5 V6 E6 V7 E7] B)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 779, clojure.core/doseq 21933},
             (doseq [V0 E0 V1 E1 V2 E2 V3 E3 V4 E4 V5 E5 V6 E6 V7 E7 V8 E8] B)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 876, clojure.core/doseq 43949}})))
  (testing "(doseq [V E] B0 ... B8)"
    (is (= (into (sorted-map-by (fn [l r] (compare (count-subforms l) (count-subforms r))))
                 (map (fn [ncolls]
                        (let [binder '[V E]
                              body (mapv (fn [i] (symbol (str "B" i))) (range ncolls))
                              ->form (fn [op] (list* op binder body))
                              ->count (fn [op] (count-subforms (macroexpand-1 (->form op))))]
                          {(->form 'doseq)
                           (into {} (map (fn [op] {op (->count op)}))
                                 `#{c/doseq fixed/doseq})})))
                 (range 10))
           '{(doseq [V E])
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 99,  clojure.core/doseq 87},
             (doseq [V E] B0)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 100, clojure.core/doseq 89},
             (doseq [V E] B0 B1)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 101, clojure.core/doseq 91},
             (doseq [V E] B0 B1 B2)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 102, clojure.core/doseq 93},
             (doseq [V E] B0 B1 B2 B3)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 103, clojure.core/doseq 95},
             (doseq [V E] B0 B1 B2 B3 B4)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 104, clojure.core/doseq 97},
             (doseq [V E] B0 B1 B2 B3 B4 B5)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 105, clojure.core/doseq 99},
             (doseq [V E] B0 B1 B2 B3 B4 B5 B6)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 106, clojure.core/doseq 101},
             (doseq [V E] B0 B1 B2 B3 B4 B5 B6 B7)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 107, clojure.core/doseq 103},
             (doseq [V E] B0 B1 B2 B3 B4 B5 B6 B7 B8)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 108, clojure.core/doseq 105}})))
  (testing "(doseq [V0 E0 V1 E1] B0 ... B8)"
    (is (= (into (sorted-map-by (fn [l r] (compare (count-subforms l) (count-subforms r))))
                 (map (fn [ncolls]
                        (let [binder '[V0 E0 V1 E1]
                              body (mapv (fn [i] (symbol (str "B" i))) (range ncolls))
                              ->form (fn [op] (list* op binder body))
                              ->count (fn [op] (count-subforms (macroexpand-1 (->form op))))]
                          {(->form 'doseq)
                           (into {} (map (fn [op] {op (->count op)}))
                                 `#{c/doseq fixed/doseq})})))
                 (range 10))
           '{(doseq [V0 E0 V1 E1])
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 196, clojure.core/doseq 257},
             (doseq [V0 E0 V1 E1] B0)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 197, clojure.core/doseq 261},
             (doseq [V0 E0 V1 E1] B0 B1)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 198, clojure.core/doseq 265},
             (doseq [V0 E0 V1 E1] B0 B1 B2)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 199, clojure.core/doseq 269},
             (doseq [V0 E0 V1 E1] B0 B1 B2 B3)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 200, clojure.core/doseq 273},
             (doseq [V0 E0 V1 E1] B0 B1 B2 B3 B4)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 201, clojure.core/doseq 277},
             (doseq [V0 E0 V1 E1] B0 B1 B2 B3 B4 B5)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 202, clojure.core/doseq 281},
             (doseq [V0 E0 V1 E1] B0 B1 B2 B3 B4 B5 B6)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 203, clojure.core/doseq 285},
             (doseq [V0 E0 V1 E1] B0 B1 B2 B3 B4 B5 B6 B7)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 204, clojure.core/doseq 289},
             (doseq [V0 E0 V1 E1] B0 B1 B2 B3 B4 B5 B6 B7 B8)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 205, clojure.core/doseq 293}})))
  (testing "(doseq [V0 E0 V1 E1 V2 E2 V3 E3 V4 E4 V5 E5 V6 E6 V7 E7 V8 E8] B0 ... B8)"
    (is (= (into (sorted-map-by (fn [l r] (compare (count-subforms l) (count-subforms r))))
                 (map (fn [ncolls]
                        (let [binder '[V0 E0 V1 E1 V2 E2 V3 E3 V4 E4 V5 E5 V6 E6 V7 E7 V8 E8]
                              body (mapv (fn [i] (symbol (str "B" i))) (range ncolls))
                              ->form (fn [op] (list* op binder body))
                              ->count (fn [op] (count-subforms (macroexpand-1 (->form op))))]
                          {(->form 'doseq)
                           (into {} (map (fn [op] {op (->count op)}))
                                 `#{c/doseq fixed/doseq})})))
                 (range 10))
           '{(doseq [V0 E0 V1 E1 V2 E2 V3 E3 V4 E4 V5 E5 V6 E6 V7 E7 V8 E8])
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 875, clojure.core/doseq 43437},
             (doseq [V0 E0 V1 E1 V2 E2 V3 E3 V4 E4 V5 E5 V6 E6 V7 E7 V8 E8] B0)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 876, clojure.core/doseq 43949},
             (doseq [V0 E0 V1 E1 V2 E2 V3 E3 V4 E4 V5 E5 V6 E6 V7 E7 V8 E8] B0 B1)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 877, clojure.core/doseq 44461},
             (doseq [V0 E0 V1 E1 V2 E2 V3 E3 V4 E4 V5 E5 V6 E6 V7 E7 V8 E8] B0 B1 B2)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 878, clojure.core/doseq 44973},
             (doseq [V0 E0 V1 E1 V2 E2 V3 E3 V4 E4 V5 E5 V6 E6 V7 E7 V8 E8] B0 B1 B2 B3)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 879, clojure.core/doseq 45485},
             (doseq [V0 E0 V1 E1 V2 E2 V3 E3 V4 E4 V5 E5 V6 E6 V7 E7 V8 E8] B0 B1 B2 B3 B4)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 880, clojure.core/doseq 45997},
             (doseq [V0 E0 V1 E1 V2 E2 V3 E3 V4 E4 V5 E5 V6 E6 V7 E7 V8 E8] B0 B1 B2 B3 B4 B5)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 881, clojure.core/doseq 46509},
             (doseq [V0 E0 V1 E1 V2 E2 V3 E3 V4 E4 V5 E5 V6 E6 V7 E7 V8 E8] B0 B1 B2 B3 B4 B5 B6)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 882, clojure.core/doseq 47021},
             (doseq [V0 E0 V1 E1 V2 E2 V3 E3 V4 E4 V5 E5 V6 E6 V7 E7 V8 E8] B0 B1 B2 B3 B4 B5 B6 B7)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 883, clojure.core/doseq 47533},
             (doseq [V0 E0 V1 E1 V2 E2 V3 E3 V4 E4 V5 E5 V6 E6 V7 E7 V8 E8] B0 B1 B2 B3 B4 B5 B6 B7 B8)
             {io.github.frenchy64.fully-satisfies.linear-expansion/doseq 884, clojure.core/doseq 48045}}))))

(deftest for-expands-exponentially-test
  (is (thrown? Exception (eval `(c/for [] (counting-macro)))))
  (is-count-expansions 2 `(c/for [_# nil] (counting-macro)))
  (is-count-expansions 2 `(c/for [_# nil _# nil _# nil _# nil] (counting-macro)))
  (is-count-expansions 1 `(c/for [_# nil _# nil _# nil _# (counting-macro)] _#))
  (is-count-expansions 4 `(c/for [_# (counting-macro) _# (counting-macro) _# (counting-macro) _# (counting-macro)] _#))
  (is-count-expansions 16 `(c/for [_# nil] (c/for [_# nil] (c/for [_# nil] (c/for [_# nil] (counting-macro))))))
  (is-count-expansions 64 `(c/for [_# nil] (c/for [_# nil] (c/for [_# nil] (c/for [_# nil] (c/for [_# nil] (c/for [_# nil] (counting-macro))))))))
  (is-count-expansions 6 `(c/for [_# (counting-macro) _# (counting-macro) _# (counting-macro) _# (counting-macro)] (counting-macro))))

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
  (is (= (fixed/for [x (range 3) y (range 3) :let [z (+ (long x) (long y))] :when (odd? z)] [x y z])
         '([0 1 1] [1 0 1] [1 2 3] [2 1 3]))))
