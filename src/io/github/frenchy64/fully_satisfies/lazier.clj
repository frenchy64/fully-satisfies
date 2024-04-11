(ns io.github.frenchy64.fully-satisfies.lazier
  (:refer-clojure :exclude [butlast drop sequence]))

(when-not (= "true" (System/getProperty "io.github.frenchy64.fully-satisfies.safer.drop.no-1.12-perf-warn"))
  (when (try (Class/forName "clojure.lang.IDrop")
             (catch Throwable _))
    (println "WARNING: io.github.frenchy64.fully-satisfies.safer/drop is missing 1.12 performance features")))

;;TODO unit test
(def 
 ^{:arglists '([coll])
   :doc "Return a seq of all but the last item in coll, in linear time"
   :added "1.0"
   :static true}
 butlast (fn ^:static butlast [s]
           ;; initialize loop with (seq s) - Ambrose
           (loop [ret [] s (seq s)]
             (if (next s)
               (recur (conj ret (first s)) (next s))
               (seq ret)))))

;;TODO unit test
;; fix comma in docstring - Ambrose
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
