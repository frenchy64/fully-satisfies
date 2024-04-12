;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.leaner
  (:refer-clojure :exclude [drop-while some]))

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
     (let [step (fn [pred s] ;; intialize coll with `seq` and use next instead of seq+first - Ambrose
                  (if (and s (pred (first s)))
                    (recur pred (next s))
                    s))]
       (lazy-seq (step pred (seq coll))))))

(defn some
  "Returns the first logical true value of (pred x) for any x in coll,
  else nil.  One common idiom is to use a set as pred, for example
  this will return :fred if :fred is in the sequence, otherwise nil:
  (some #{:fred} coll)"
  {:added "1.0"
   :static true}
  [pred coll]
  (loop [s (seq coll)] ;; only call seq at top of loop - Ambrose
    (when s
      (or (pred (first s)) (recur (next s))))))
