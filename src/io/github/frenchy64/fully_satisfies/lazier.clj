(ns io.github.frenchy64.fully-satisfies.lazier
  "Variants of clojure.core functions that are slightly lazier when
  processing and/or returning lazy seqs."
  (:refer-clojure :exclude [cycle sequence bounded-count iterator-seq dedupe]))

;;TODO unit test
;; fix comma in docstring - Ambrose
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
    per chunk."
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

(defn dedupe
  "Returns a lazy sequence removing consecutive duplicates in coll.
  Returns a transducer when no collection is provided.

  lazier/dedupe additionally:
  - does not force a lazy seq until needed
  - forces 32 elements per chunk instead of 33, preserving 32 elements
    per chunk."
  {:added "1.7"}
  ([]
   (fn [rf]
     (let [pv (volatile! ::none)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
            (let [prior @pv]
              (vreset! pv input)
              (if (= prior input)
                result
                (rf result input))))))))
  ;;use lazier/sequence - Ambrose
  ([coll] (sequence (dedupe) coll)))
