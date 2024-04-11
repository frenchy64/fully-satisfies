(ns io.github.frenchy64.fully-satisfies.lazier
  (:refer-clojure :exclude [sequence]))

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
   (io.github.frenchy64.fully_satisfies.lazier.RT/chunkIteratorSequence
     (clojure.lang.TransformerIterator/create xform (clojure.lang.RT/iter coll))))
  ([xform coll & colls]
   (io.github.frenchy64.fully_satisfies.lazier.RT/chunkIteratorSequence
     (clojure.lang.TransformerIterator/createMulti
       xform
       (map #(clojure.lang.RT/iter %) (cons coll colls))))))
