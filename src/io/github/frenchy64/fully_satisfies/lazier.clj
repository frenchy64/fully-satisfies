(ns io.github.frenchy64.fully-satisfies.lazier
  (:refer-clojure :exclude [butlast cycle drop sequence dorun bounded-count]))

(when-not (= "true" (System/getProperty "io.github.frenchy64.fully-satisfies.safer.drop.no-1.12-perf-warn"))
  (when (try (Class/forName "clojure.lang.IDrop")
             (catch Throwable _))
    (println "WARNING: io.github.frenchy64.fully-satisfies.safer/drop is missing 1.12 performance features")))

(defn drop
  "Returns a laziness-preserving sequence of all but the first n items in coll.
  Returns a stateful transducer when no collection is provided."
  {:added "1.0"
   :static true}
  ([n]
     (fn [rf]
       (let [nv (volatile! n)]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
              (let [n @nv]
                (vswap! nv dec)
                (if (pos? n)
                  result
                  (rf result input))))))))
  ([n coll]
     (do #_if #_(instance? clojure.lang.IDrop coll)
       #_(or
        (if (pos? n)
          (.drop ^clojure.lang.IDrop coll (if (int? n) n (Math/ceil n)))
          (seq coll))
        ())
       (let [step (fn [n coll]
                    (if (pos? n) ;; don't check seq if not pos? - Ambrose
                      (when-let [s (next coll)] ;; call next instead of seq+rest
                        (recur (dec n) s))
                      coll))]
         (lazy-seq (step n coll))))))

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
   (when (pos? n) ;; lazier, test pos? first - Ambrose
     (when-let [coll (next coll)] ;; leaner, just call next instead of seq+next - Ambrose
       (recur (dec n) coll)))))

(defn bounded-count
  "If coll is counted? returns its count, else will count at most the first n
  elements of coll using its seq"
  {:added "1.9"}
  [n coll]
  (if (counted? coll)
    (count coll)
    (if (pos? n) ;; don't force seq if n==0 - Ambrose
      (loop [i 0 s (seq coll)]
        (if (and s (< i n))
          (recur (inc i) (next s))
          i))
      0)))
