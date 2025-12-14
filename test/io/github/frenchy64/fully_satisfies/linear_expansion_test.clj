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

(deftest doseq-expansion-comparison-test
  (testing '(doseq [] B)
    (is (= (pretty-doseq-expansion (macroexpand-1 `(doseq [] ~'B)))
           (pretty-doseq-expansion (macroexpand-1 `(fixed/doseq [] ~'B)))
           '(do B))))
  (testing '(doseq [V E] B)
    (testing 'c/doseq
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
      (is (= (pretty-doseq-expansion (macroexpand-1 `(fixed/doseq [~'V ~'E] ~'B)))
             '(loop [s (seq E) chunk- nil count- 0 i 0]
                (let [in-chunk (< i count-)
                      s (if in-chunk s (seq s))]
                  (when (if in-chunk true s)
                    (let [chunked? (if in-chunk false (chunked-seq? s))
                          V (if in-chunk (.nth chunk- i) (if chunked? nil (first s)))]
                      (if (if in-chunk false chunked?)
                        (let [f (chunk-first s)]
                          (recur (chunk-rest s) f (int (count f)) (int 0)))
                        (do (do B)
                            (if in-chunk
                              (recur s chunk- count- (unchecked-inc i))
                              (recur (next s) nil 0 0))))))))))))
  (testing '(doseq [:when P] B)
    (is (= (pretty-doseq-expansion (macroexpand-1 `(doseq [:when ~'P] ~'B)))
           (pretty-doseq-expansion (macroexpand-1 `(fixed/doseq [:when ~'P] ~'B)))
           '(if P (do (do B) nil) nil))))
  (testing '(doseq [V E :when P] B)
    (testing 'c/doseq
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
      (is (= (pretty-doseq-expansion (macroexpand-1 `(fixed/doseq [~'V ~'E :when ~'P] ~'B)))
             '(loop [s (seq E) chunk- nil count- 0 i 0]
                (let [in-chunk (< i count-) s (if in-chunk s (seq s))]
                  (when (if in-chunk true s)
                    (let [chunked? (if in-chunk false (chunked-seq? s))
                          V (if in-chunk (.nth chunk- i) (if chunked? nil (first s)))]
                      (if (if in-chunk false chunked?)
                        (let [f (chunk-first s)]
                          (recur (chunk-rest s) f (int (count f)) (int 0)))
                        (do (if P
                              (do (do B)
                                  (if
                                    in-chunk
                                    (recur s chunk- count- (unchecked-inc i))
                                    (recur (next s) nil 0 0)))
                              (if in-chunk
                                (recur s chunk- count- (unchecked-inc i))
                                (recur (next s) nil 0 0)))))))))))))

  (testing '(doseq [:while P] B)
    (is (= (pretty-doseq-expansion (macroexpand-1 `(doseq [:while ~'P] ~'B)))
           (pretty-doseq-expansion (macroexpand-1 `(fixed/doseq [:while ~'P] ~'B)))
           '(when P (do B) nil))))
  (testing '(doseq [V E :while P] B)
    (testing 'c/doseq
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
      (is (= (pretty-doseq-expansion (macroexpand-1 `(fixed/doseq [~'V ~'E :while ~'P] ~'B)))
             '(loop [s (seq E) chunk- nil count- 0 i 0]
                (let [in-chunk (< i count-) s (if in-chunk s (seq s))]
                  (when (if in-chunk true s)
                    (let [chunked? (if in-chunk false (chunked-seq? s))
                          V (if in-chunk (.nth chunk- i) (if chunked? nil (first s)))]
                      (if (if in-chunk false chunked?)
                        (let [f (chunk-first s)]
                          (recur (chunk-rest s) f (int (count f)) (int 0)))
                        (do (when P
                              (do B)
                              (if in-chunk
                                (recur s chunk- count- (unchecked-inc i))
                                (recur (next s) nil 0 0)))))))))))))
  (testing '(doseq [:let ['S 'T]] B)
    (is (= (pretty-doseq-expansion (macroexpand-1 `(fixed/doseq [:let [~'S ~'T]] ~'B)))
           (pretty-doseq-expansion (macroexpand-1 `(doseq [:let [~'S ~'T]] ~'B)))
           '(let [S T] (do B)))))
  (testing '(doseq [V0 E0 V1 E1] B)
    (testing 'c/doseq
      (is (= (pretty-doseq-expansion (macroexpand-1 `(doseq ~'[V0 E0 E1 V1] ~'B)))
             '(loop [s (seq E0) chunk- nil count- 0 i 0]
                (if (< i count-)
                  (let [V0 (.nth chunk- i)]
                    (loop [s (seq V1) chunk- nil count- 0 i 0]
                      (if (< i count-)
                        (let [E1 (.nth chunk- i)]
                          (do B)
                          (recur s chunk- count- (unchecked-inc i)))
                        (when-let [s (seq s)]
                          (if (chunked-seq? s)
                            (let [f (chunk-first s)]
                              (recur (chunk-rest s) f (int (count f)) (int 0)))
                            (let [E1 (first s)]
                              (do B)
                              (recur (next s) nil 0 0))))))
                    (recur s chunk- count- (unchecked-inc i)))
                  (when-let [s (seq s)]
                    (if (chunked-seq? s)
                      (let [f (chunk-first s)]
                        (recur (chunk-rest s) f (int (count f)) (int 0)))
                      (let [V0 (first s)]
                        (loop [s (seq V1) chunk- nil count- 0 i 0]
                          (if (< i count-)
                            (let [E1 (.nth chunk- i)]
                              (do B)
                              (recur s chunk- count- (unchecked-inc i)))
                            (when-let [s (seq s)]
                              (if (chunked-seq? s)
                                (let [f (chunk-first s)]
                                  (recur (chunk-rest s) f (int (count f)) (int 0)))
                                (let [E1 (first s)]
                                  (do B)
                                  (recur (next s) nil 0 0))))))
                        (recur (next s) nil 0 0)))))))))
    (testing 'fixed/doseq
      (is (= (pretty-doseq-expansion (macroexpand-1 `(fixed/doseq ~'[V0 E0 E1 V1] ~'B)))
             '(loop [s (seq E0) chunk- nil count- 0 i 0]
                (let [in-chunk (< i count-)
                      s (if in-chunk s (seq s))]
                  (when (if in-chunk true s)
                    (let [chunked? (if in-chunk false (chunked-seq? s))
                          V0 (if in-chunk (.nth chunk- i) (if chunked? nil (first s)))]
                      (if (if in-chunk false chunked?)
                        (let [f (chunk-first s)]
                          (recur (chunk-rest s) f (int (count f)) (int 0)))
                        (do (loop
                              [s (seq V1) chunk- nil count- 0 i 0]
                              (let [in-chunk (< i count-)
                                    s (if in-chunk s (seq s))]
                                (when (if in-chunk true s)
                                  (let [chunked? (if in-chunk false (chunked-seq? s))
                                        E1 (if in-chunk (.nth chunk- i) (if chunked? nil (first s)))]
                                    (if (if in-chunk false chunked?)
                                      (let [f (chunk-first s)]
                                        (recur (chunk-rest s) f (int (count f)) (int 0)))
                                      (do (do B)
                                          (if in-chunk
                                            (recur s chunk- count- (unchecked-inc i))
                                            (recur (next s) nil 0 0))))))))
                            (if in-chunk
                              (recur s chunk- count- (unchecked-inc i))
                              (recur (next s) nil 0 0))))))))))))
  (testing '(doseq [V0 E0 V1 E1 V2 E2] B)
    (testing 'c/doseq
      (is (= (pretty-doseq-expansion (macroexpand-1 `(doseq ~'[V0 E0 E1 V1 E2 V2] ~'B)))
             '(loop [s (seq E0) chunk- nil count- 0 i 0]
                (if (< i count-)
                  (let [V0 (.nth chunk- i)]
                    (loop [s (seq V1) chunk- nil count- 0 i 0]
                      (if (< i count-)
                        (let [E1 (.nth chunk- i)]
                          (loop [s (seq V2) chunk- nil count- 0 i 0]
                            (if (< i count-)
                              (let [E2 (.nth chunk- i)]
                                (do B)
                                (recur s chunk- count- (unchecked-inc i)))
                              (when-let [s (seq s)]
                                (if (chunked-seq? s)
                                  (let [f (chunk-first s)]
                                    (recur (chunk-rest s) f (int (count f)) (int 0)))
                                  (let [E2 (first s)]
                                    (do B)
                                    (recur (next s) nil 0 0))))))
                          (recur s chunk- count- (unchecked-inc i)))
                        (when-let [s (seq s)]
                          (if (chunked-seq? s)
                            (let [f (chunk-first s)]
                              (recur (chunk-rest s) f (int (count f)) (int 0)))
                            (let [E1 (first s)]
                              (loop [s (seq V2) chunk- nil count- 0 i 0]
                                (if (< i count-)
                                  (let [E2 (.nth chunk- i)]
                                    (do B)
                                    (recur s chunk- count- (unchecked-inc i)))
                                  (when-let [s (seq s)]
                                    (if (chunked-seq? s)
                                      (let [f (chunk-first s)]
                                        (recur (chunk-rest s) f (int (count f)) (int 0)))
                                      (let [E2 (first s)]
                                        (do B)
                                        (recur (next s) nil 0 0))))))
                              (recur (next s) nil 0 0))))))
                    (recur s chunk- count- (unchecked-inc i)))
                  (when-let [s (seq s)]
                    (if (chunked-seq? s)
                      (let [f (chunk-first s)]
                        (recur (chunk-rest s) f (int (count f)) (int 0)))
                      (let [V0 (first s)]
                        (loop [s (seq V1) chunk- nil count- 0 i 0]
                          (if (< i count-)
                            (let [E1 (.nth chunk- i)]
                              (loop [s (seq V2) chunk- nil count- 0 i 0]
                                (if (< i count-)
                                  (let [E2 (.nth chunk- i)]
                                    (do B)
                                    (recur s chunk- count- (unchecked-inc i)))
                                  (when-let [s (seq s)]
                                    (if (chunked-seq? s)
                                      (let [f (chunk-first s)]
                                        (recur (chunk-rest s) f (int (count f)) (int 0)))
                                      (let [E2 (first s)]
                                        (do B)
                                        (recur (next s) nil 0 0))))))
                              (recur s chunk- count- (unchecked-inc i)))
                            (when-let [s (seq s)]
                              (if (chunked-seq? s)
                                (let [f (chunk-first s)]
                                  (recur (chunk-rest s) f (int (count f)) (int 0)))
                                (let [E1 (first s)]
                                  (loop [s (seq V2) chunk- nil count- 0 i 0]
                                    (if (< i count-)
                                      (let [E2 (.nth chunk- i)]
                                        (do B)
                                        (recur s chunk- count- (unchecked-inc i)))
                                      (when-let [s (seq s)]
                                        (if (chunked-seq? s)
                                          (let [f (chunk-first s)]
                                            (recur (chunk-rest s) f (int (count f)) (int 0)))
                                          (let [E2 (first s)]
                                            (do B)
                                            (recur (next s) nil 0 0))))))
                                  (recur (next s) nil 0 0))))))
                        (recur (next s) nil 0 0)))))))))
    (testing 'fixed/doseq
      (is (= (pretty-doseq-expansion (macroexpand-1 `(fixed/doseq ~'[V0 E0 E1 V1 E2 V2] ~'B)))
             '(loop [s (seq E0) chunk- nil count- 0 i 0]
                (let [in-chunk (< i count-)
                      s (if in-chunk s (seq s))]
                  (when (if in-chunk true s)
                    (let [chunked? (if in-chunk false (chunked-seq? s))
                          V0 (if in-chunk (.nth chunk- i) (if chunked? nil (first s)))]
                      (if (if in-chunk false chunked?)
                        (let [f (chunk-first s)]
                          (recur (chunk-rest s) f (int (count f)) (int 0)))
                        (do (loop [s (seq V1) chunk- nil count- 0 i 0]
                              (let [in-chunk (< i count-)
                                    s (if in-chunk s (seq s))]
                                (when (if in-chunk true s)
                                  (let [chunked? (if in-chunk false (chunked-seq? s))
                                        E1 (if in-chunk (.nth chunk- i) (if chunked? nil (first s)))]
                                    (if (if in-chunk false chunked?)
                                      (let [f (chunk-first s)]
                                        (recur (chunk-rest s) f (int (count f)) (int 0)))
                                      (do (loop [s (seq V2) chunk- nil count- 0 i 0]
                                            (let [in-chunk (< i count-)
                                                  s (if in-chunk s (seq s))]
                                              (when
                                                (if in-chunk true s)
                                                (let [chunked? (if in-chunk false (chunked-seq? s))
                                                      E2 (if in-chunk (.nth chunk- i) (if chunked? nil (first s)))]
                                                  (if (if in-chunk false chunked?)
                                                    (let [f (chunk-first s)]
                                                      (recur (chunk-rest s) f (int (count f)) (int 0)))
                                                    (do (do B)
                                                        (if in-chunk
                                                          (recur s chunk- count- (unchecked-inc i))
                                                          (recur (next s) nil 0 0))))))))
                                          (if in-chunk
                                            (recur s chunk- count- (unchecked-inc i))
                                            (recur (next s) nil 0 0))))))))
                            (if in-chunk
                              (recur s chunk- count- (unchecked-inc i))
                              (recur (next s) nil 0 0)))))))))))))

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
  (is (= (fixed/for [x (range 3) y (range 3) :let [z (+ x y)] :when (odd? z)] [x y z])
         '([0 1 1] [1 0 1] [1 2 3] [2 1 3]))))
