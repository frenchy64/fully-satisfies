;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.lazier
  "Variants of clojure.core functions that are slightly lazier when
  processing and/or returning lazy seqs.
  
  Some common (anti)patterns were to blame for the less-lazy versions of
  these functions. The main insight was that if you are using a counter to
  bound how many elements of a seq you are walking, you can be lazier by
  testing the counter _before_ testing for more elements. For example, 

  ```java
  int n = 0;
  while(iter.hasNext() && n < CHUNK_SIZE)
  ```

  will force n+1 elements and

  ```java
  int n = 0;
  while(n < CHUNK_SIZE && iter.hasNext())
  ```

  will force n elements. In this case, realizing the extra element has no utility because
  the chunk only fits CHUNK_SIZE elements.

  Another problematic pattern was using seq-based implementations for sequence functions.
  Seqs must be non-empty, but a sequence can be empty, so implementations can be lazier."
  (:refer-clojure :exclude [bounded-count cycle dedupe iterator-seq sequence]))

;;TODO unit test
;; fix comma in docstring after "(sequence nil) yields ()" - Ambrose
;; https://clojure.atlassian.net/browse/CLJ-2795
(defn sequence
  "Coerces coll to a (possibly empty) sequence, if it is not already
  one. Will not force a lazy seq. (sequence nil) yields (). When a
  transducer is supplied, returns a lazy sequence of applications of
  the transform to the items in coll(s), i.e. to the set of first
  items of each coll, followed by the set of second
  items in each coll, until any one of the colls is exhausted.  Any
  remaining items in other colls are ignored. The transform should accept
  number-of-colls arguments.
  
  lazier/sequence additionally:
  - avoids realizing the first element of the return until it is needed.
  - realizes 32 elements per chunk instead of 33, preserving 32 elements
    per chunk.
  - fixes a comma in the docstring."
  {:added "1.0"
   :static true}
  ([coll]
   (if (seq? coll)
     coll
     (or (seq coll) ())))
  ([xform coll]
   ;; return a sequence instead of seq - Ambrose
   (io.github.frenchy64.fully_satisfies.lazier.RT/chunkIteratorSequence
     (clojure.lang.TransformerIterator/create xform (clojure.lang.RT/iter coll))))
  ([xform coll & colls]
   ;; return a sequence instead of seq - Ambrose
   (io.github.frenchy64.fully_satisfies.lazier.RT/chunkIteratorSequence
     (clojure.lang.TransformerIterator/createMulti
       xform
       (map #(clojure.lang.RT/iter %) (cons coll colls))))))

(defn cycle
  "Returns a lazy (infinite!) sequence of repetitions of the items in coll.
  
  lazier/cycle does not realize a lazy seq coll until it is needed."
  {:added "1.0"
   :static true}
  [coll] (if (seq? coll) ;; wrap in lazy-seq if seq - Ambrose
           (lazy-seq (clojure.lang.Cycle/create (seq coll)))
           (clojure.lang.Cycle/create (seq coll))))

(defn bounded-count
  "If coll is counted? returns its count, else will count at most the first n
  elements of coll using its seq.
  
  lazier/bounded-count additionally:
  - does not force coll if n==0
  - forces at most n elements of a lazy seq instead of n+1."
  {:added "1.9"}
  [n coll]
  (if (counted? coll)
    (count coll)
    (if (pos? n)
      (loop [i 0 s (seq coll)]
        (if s
          (let [i (inc i)]
            (cond-> i
              (< i n) (recur (next s))))
          i))
      0)))

(defn bounded-count'
  "If coll is counted? returns its count, else will count at most the first n
  elements of coll using its seq, short-circuiting if a counted? seq is found
  within n-1 nexts.
  
  lazier/bounded-count' additionally:
  - does not force coll if n==0
  - forces at most n elements of a lazy seq instead of n+1."
  [n coll]
  (if (counted? coll)
    (count coll)
    (if (pos? n)
      (loop [i 0 s (seq coll)]
        (if s
          (if (counted? s)
            (+ i (count s))
            (let [i (inc i)]
              (cond-> i
                (< i n) (recur (next s)))))
          i))
      0)))

;; note: clojure.core/seq has a similar code path for Iterables
(defn iterator-seq
  "Returns a seq on a java.util.Iterator. Note that most collections
  providing iterators implement Iterable and thus support seq directly.
  Seqs cache values, thus iterator-seq should not be used on any
  iterator that repeatedly returns the same mutable object.
  
  lazier/iterator-seq additionally:
  - forces 32 elements per chunk instead of 33, preserving 32 elements
    per chunk."
  {:added "1.0"
   :static true}
  [iter]
  (io.github.frenchy64.fully_satisfies.lazier.RT/chunkIteratorSeq iter))

(let [none (Object.)]
  (defn dedupe
    "Returns a lazy sequence removing consecutive duplicates in coll.
    Returns a transducer when no collection is provided.

    lazier/dedupe additionally:
    - does not force a lazy seq until needed
    - forces 32 elements per chunk instead of 33, preserving 32 elements
    per chunk.
    - transducer arity behaves correctly for all inputs (including :clojure.core/none)"
    {:added "1.7"}
    ([]
     (fn [rf]
       (let [pv (volatile! none)]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
            (let [prior @pv]
              (vreset! pv input)
              (if (= prior input)
                result
                (rf result input))))))))
    ;; use lazier/sequence - Ambrose
    ;; allow :clojure.core/none - Ambrose
    ([coll] (sequence (dedupe) coll))))
