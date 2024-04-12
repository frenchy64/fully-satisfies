(ns io.github.frenchy64.fully-satisfies.cornerless
  "Variants of clojure.core functions that don't have obscure corner cases."
  (:refer-clojure :exclude [partition-by]))

(let [none (Object.)]
  (defn partition-by
    "Applies f to each value in coll, splitting it each time f returns a
     new value.  Returns a lazy seq of partitions.  Returns a stateful
     transducer when no collection is provided.
    
     Additionally, the cornerless/partition-by transducer behaves correctly for
     all values (including :clojure.core/none)."
    {:added "1.2"
     :static true}
    ([f]
    (fn [rf]
      (let [a (java.util.ArrayList.)
            pv (volatile! none)]
        (fn
          ([] (rf))
          ([result]
             (let [result (if (.isEmpty a)
                            result
                            (let [v (vec (.toArray a))]
                              ;;clear first!
                              (.clear a)
                              (unreduced (rf result v))))]
               (rf result)))
          ([result input]
             (let [pval @pv
                   val (f input)]
               (vreset! pv val)
               (if (or (identical? pval none)
                       (= val pval))
                 (do
                   (.add a input)
                   result)
                 (let [v (vec (.toArray a))]
                   (.clear a)
                   (let [ret (rf result v)]
                     (when-not (reduced? ret)
                       (.add a input))
                     ret)))))))))
    ([f coll]
       (lazy-seq
        (when-let [s (seq coll)]
          (let [fst (first s)
                fv (f fst)
                run (cons fst (take-while #(= fv (f %)) (next s)))]
            (cons run (partition-by f (lazy-seq (drop (count run) s))))))))))
