(ns io.github.frenchy64.fully-satisfies.leaner
  (:refer-clojure :exclude [drop-while]))

(defn drop-while
  "Returns a lazy sequence of the items in coll starting from the
  first item for which (pred item) returns logical false.  Returns a
  stateful transducer when no collection is provided."
  {:added "1.0"
   :static true}
  ([pred]
     (fn [rf]
       (let [dv (volatile! true)]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
              (let [drop? @dv]
                (if (and drop? (pred input))
                  result
                  (do
                    (vreset! dv nil)
                    (rf result input)))))))))
  ([pred coll]
     (let [step (fn [pred coll] ;; intialize coll with `seq` and use next instead of seq+first - Ambrose
                  (if (and s (pred (first s)))
                    (recur pred (next s))
                    s))]
       (lazy-seq (step pred (seq coll))))))
