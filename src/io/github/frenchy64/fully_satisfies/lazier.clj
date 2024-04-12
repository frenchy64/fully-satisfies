(ns io.github.frenchy64.fully-satisfies.lazier
  (:refer-clojure :exclude [cycle sequence dorun bounded-count
                            iterator-seq dedupe]))

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
  number-of-colls arguments"
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
  "Returns a lazy (infinite!) sequence of repetitions of the items in coll."
  {:added "1.0"
   :static true}
  [coll] (if (seq? coll) ;; wrap in lazy-seq if seq - Ambrose
           (lazy-seq (clojure.lang.Cycle/create (seq coll)))
           (clojure.lang.Cycle/create (seq coll))))

(defn dorun
  "When lazy sequences are produced via functions that have side
  effects, any effects other than those needed to produce the first
  element in the seq do not occur until the seq is consumed. dorun can
  be used to force any effects. Walks through the successive nexts of
  the seq, does not retain the head and returns nil."
  {:added "1.0"
   :static true}
  ([coll]
   (when-let [s (next coll)] ;; leaner, just call next instead of seq+next - Ambrose
     (recur s)))
  ([n coll]
   (when (pos? n)
     (loop [n (dec n) s (seq coll)]
       (when (and (pos? n) s)
         (recur (dec n) (next s)))))))

(defn bounded-count
  "If coll is counted? returns its count, else will count at most the first n
  elements of coll using its seq"
  {:added "1.9"}
  [n coll]
  (if (counted? coll)
    (count coll)
    (if (pos? n) ;; don't force seq if n==0 - Ambrose
      (loop [i 1 s (seq coll)] ;; start i at 1 to skip extra next - Ambrose
        (if (and s (< i n))
          (recur (inc i) (next s))
          i))
      0)))

(defn iterator-seq
  "Returns a seq on a java.util.Iterator. Note that most collections
  providing iterators implement Iterable and thus support seq directly.
  Seqs cache values, thus iterator-seq should not be used on any
  iterator that repeatedly returns the same mutable object."
  {:added "1.0"
   :static true}
  [iter]
  (io.github.frenchy64.fully_satisfies.lazier.RT/chunkIteratorSeq iter))

(defn dedupe
  "Returns a lazy sequence removing consecutive duplicates in coll.
  Returns a transducer when no collection is provided."
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
