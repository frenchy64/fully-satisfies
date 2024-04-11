(ns io.github.frenchy64.fully-satisfies.safer
  (:refer-clojure :exclude [butlast every? split-at split-with take-last nthrest sort drop-last
                            sort-by splitv-at partitionv-all last])
  (:require [io.github.frenchy64.fully-satisfies.lazier :as lazier]))

;;TODO unit test
(defn every?
  "Returns true if (pred x) is logical true for every x in coll, else
  false."
  {:tag Boolean
   :added "1.0"
   :static true}
  [pred coll]
  ;; reuses result of `seq` - Ambrose
  (if-let [coll (seq coll)]
    (if (pred (first coll))
      (recur pred (next coll))
      false)
    true))

(defn drop-last
  "Return a lazy sequence of all but the last n (default 1) items in coll"
  {:added "1.0"
   :static true}
  ([coll] (drop-last 1 coll))
  ([n coll] (let [coll (lazier/sequence coll)] ;; bind a sequence - Ambrose
              (map (fn [x _] x) coll (lazier/drop n coll)))))

(defn take-last
  "Returns a seq of the last n items in coll.  Depending on the type
  of coll may be no better than linear time.  For vectors, see also subvec."
  {:added "1.1"
   :static true}
  [n coll]
  (let [s (seq coll)] ;; pull seq call before loop initialization - Ambrose
    (loop [s s, lead (seq (drop n s))]
      (if lead
        (recur (next s) (next lead))
        s))))

(defn split-at
  "Returns a vector of [(take n coll) (drop n coll)]"
  {:added "1.0"
   :static true}
  [n coll]
  (let [coll (seq coll)] ;; call seq on `coll` - Ambrose
    [(take n coll) (drop n coll)]))

(defn split-with
  "Returns a vector of [(take-while pred coll) (drop-while pred coll)]"
  {:added "1.0"
   :static true}
  [pred coll]
  (let [coll (seq coll)] ;; call seq on `coll` - Ambrose
    [(take-while pred coll) (drop-while pred coll)]))

(defn sort
  "Returns a sorted sequence of the items in coll. If no comparator is
  supplied, uses compare.  comparator must implement
  java.util.Comparator.  Guaranteed to be stable: equal elements will
  not be reordered.  If coll is a Java array, it will be modified.  To
  avoid this, sort a copy of the array."
  {:added "1.0"
   :static true}
  ([coll]
   (sort compare coll))
  ([^java.util.Comparator comp coll]
   (if (seq coll)
     (let [a (to-array coll)]
       (. java.util.Arrays (sort a comp))
       (with-meta (or (seq a) ()) ;; in case mutated between seq and to-array - Ambrose
                  (meta coll)))
     ())))

(defn sort-by
  "Returns a sorted sequence of the items in coll, where the sort
  order is determined by comparing (keyfn item).  If no comparator is
  supplied, uses compare.  comparator must implement
  java.util.Comparator.  Guaranteed to be stable: equal elements will
  not be reordered.  If coll is a Java array, it will be modified.  To
  avoid this, sort a copy of the array."
  {:added "1.0"
   :static true}
  ([keyfn coll]
   (sort-by keyfn compare coll))
  ([keyfn ^java.util.Comparator comp coll]
   ;; use safer/sort - Ambrose
   (sort (fn [x y] (. comp (compare (keyfn x) (keyfn y)))) coll)))

(defn splitv-at
  "Returns a vector of [(into [] (take n) coll) (drop n coll)]"
  {:added "1.12"}
  [n coll]
  (let [coll (seq coll)] ;; bind seq - Ambrose
    [(into [] (take n) coll) (drop n coll)]))

(defn partitionv-all
  "Returns a lazy sequence of vector partitions, but may include
  partitions with fewer than n items at the end.
  Returns a stateful transducer when no collection is provided."
  {:added "1.12"}
  ([n]
   (partition-all n))
  ([n coll]
   (partitionv-all n n coll))
  ([n step coll]
   (lazy-seq
     (when-let [s (seq coll)]
       (let [seg (into [] (take n) s)] ;; use s not coll - Ambrose
         (cons seg (partitionv-all n step (drop step s))))))))

(def 
 ^{:arglists '([coll])
   :doc "Return the last item in coll, in linear time"
   :added "1.0"
   :static true}
 last (fn ^:static last [s]
        (let [s (seq s)] ;; call seq at top - Ambrose
          (if (next s)
            (recur (next s))
            (first s)))))
