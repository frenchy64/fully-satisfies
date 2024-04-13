;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.uniform
  "Variants of clojure.core functions that are generalized
  to work uniformly for all values."
  (:refer-clojure :exclude [partition-by halt-when]))

(let [none (Object.)]
  (defn partition-by
    "Applies f to each value in coll, splitting it each time f returns a
     new value.  Returns a lazy seq of partitions.  Returns a stateful
     transducer when no collection is provided.
    
     Additionally, the cornerless/partition-by transducer behaves uniformly for
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

;;TODO figure out if this corner case is possible
(let [halt (Object.)]
  (defn halt-when
    "Returns a transducer that ends transduction when pred returns true
    for an input. When retf is supplied it must be a fn of 2 arguments -
    it will be passed the (completed) result so far and the input that
    triggered the predicate, and its return value (if it does not throw
    an exception) will be the return value of the transducer. If retf
    is not supplied, the input that triggered the predicate will be
    returned. If the predicate never returns true the transduction is
    unaffected.
    
    cornerless/halt-when also works uniformly for all values (including
    returning a map with key :clojure.core/halt)."
    {:added "1.9"}
    ([pred] (halt-when pred nil))
    ([pred retf]
       (fn [rf]
         (fn
           ([] (rf))
           ([result]
              (if (and (map? result) (contains? result halt))
                (get result halt)
                (rf result)))
           ([result input]
              (if (pred input)
                (reduced {halt (if retf (retf (rf result) input) input)})
                (rf result input))))))))
