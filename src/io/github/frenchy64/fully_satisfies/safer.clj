;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.safer
  "Variants of clojure.core functions that improve thread-safety and general robustness
  when passed mutating collections.
 
  We agree that 'Robust programs should not mutate arrays or Iterables that have seqs on them.'
  https://clojure.org/reference/sequences
  Eductions inhabit a middle ground and might be the most practical application of this namespace.
  They are designed to be walked from first to last like seqs, but each element is recomputed
  instead of being cached like persistent seqs. This becomes problematic if a sequence function
  walks its argument multiple times without first binding a seq.
  
  For example, clojure.core/split-at could disagree on the take/drop parts of
  the collection if the coll is mutated between realizing the splits.
  Here, any one call to the eduction alternates between [0 1 2 3 4 5 6 7 8 9] and
  [9 8 7 6 5 4 3 2 1 0]. However, split-at incorrectly splits the eduction as
  [[0 1 2 3 4] [4 3 2 1 0]], and safer/split-at returns one of the two correct splits:
  [[0 1 2 3 4] [5 6 7 8 9]].

  (deftest split-at-mutation-test
    (let [up-down (atom true)
          ed (eduction (map (fn [i]
                              (when (zero? i)
                                (swap! up-down not))
                              (if @up-down
                                (- 9 i)
                                i)))
                       (range 10))]
      (is (= [[0 1 2 3 4] [4 3 2 1 0]] (split-at 5 ed)))
      (is (= [[0 1 2 3 4] [5 6 7 8 9]] (safer/split-at 5 ed)))))
  
  See io.github.frenchy64.fully-satisfies.safer-test for more details.

  The basic trick here is strategically calling seq earlier on the collection argument."
  (:refer-clojure :exclude [butlast drop-last every? last not-every? nthrest partitionv-all
                            sort sort-by split-at split-with splitv-at take-last])
  (:require [io.github.frenchy64.fully-satisfies.lazier :as lazier]))

;;TODO unit test
(defn every?
  "Returns true if (pred x) is logical true for every x in coll, else
  false.
  
  safer/every? additionally is thread-safe for mutable collections."
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

(def
 ^{:tag Boolean
   :doc "Returns false if (pred x) is logical true for every x in
  coll, else true.
        
  safer/not-every? additionally is thread-safe for mutable collections."
   :arglists '([pred coll])
   :added "1.0"}
 not-every? (comp not every?)) ;; use safer/every? - Ambrose

(defn drop-last
  "Return a lazy sequence of all but the last n (default 1) items in coll
  
  safer/drop-last additionally is thread-safe for mutable collections,
  at the cost of a call to seq."
  {:added "1.0"
   :static true}
  ([coll] (drop-last 1 coll))
  ([n coll] (let [coll (cond-> coll
                         (not (coll? coll)) seq)] ;; bind a sequence - Ambrose
              (map (fn [x _] x) coll (drop n coll)))))

(defn take-last
  "Returns a seq of the last n items in coll.  Depending on the type
  of coll may be no better than linear time.  For vectors, see also subvec.
  
  safer/drop-last additionally is thread-safe for mutable collections."
  {:added "1.1"
   :static true}
  [n coll]
  (let [s (seq coll)] ;; pull seq call before loop initialization - Ambrose
    (loop [s s, lead (seq (drop n s))]
      (if lead
        (recur (next s) (next lead))
        s))))

(defn split-at
  "Returns a vector of [(take n coll) (drop n coll)]
  
  safer/split-at additionally is thread-safe for mutable collections
  and generally robust against a mutating coll at the cost of a call to seq."
  {:added "1.0"
   :static true}
  [n coll]
  (let [coll (lazier/sequence coll)] ;; call seq on `coll` - Ambrose
    [(take n coll) (drop n coll)]))

(defn split-with
  "Returns a vector of [(take-while pred coll) (drop-while pred coll)]
  
  safer/split-with additionally is thread-safe for mutable collections,
  and generally robust against a mutating coll at the cost of a call to seq."
  {:added "1.0"
   :static true}
  [pred coll]
  (let [coll (lazier/sequence coll)] ;; call seq on `coll` - Ambrose
    [(take-while pred coll) (drop-while pred coll)]))

(defn sort
  "Returns a sorted sequence of the items in coll. If no comparator is
  supplied, uses compare.  comparator must implement
  java.util.Comparator.  Guaranteed to be stable: equal elements will
  not be reordered.  If coll is a Java array, it will be modified.  To
  avoid this, sort a copy of the array.
  
  safer/sort additionally is thread-safe for mutable collections (avoids NPE)."
  {:added "1.0"
   :static true}
  ([coll]
   (sort compare coll))
  ([^java.util.Comparator comp coll]
   (if (seq coll)
     (let [a (to-array coll)]
       (. java.util.Arrays (sort a comp))
       (with-meta (or (seq a) ()) ;; in case resized between seq and to-array - Ambrose
                  (meta coll)))
     ())))

(defn sort-by
  "Returns a sorted sequence of the items in coll, where the sort
  order is determined by comparing (keyfn item).  If no comparator is
  supplied, uses compare.  comparator must implement
  java.util.Comparator.  Guaranteed to be stable: equal elements will
  not be reordered.  If coll is a Java array, it will be modified.  To
  avoid this, sort a copy of the array.
  
  safer/sort-by additionally is thread-safe for mutable collections (avoids NPE)."
  {:added "1.0"
   :static true}
  ([keyfn coll]
   (sort-by keyfn compare coll))
  ([keyfn ^java.util.Comparator comp coll]
   ;; use safer/sort - Ambrose
   (sort (fn [x y] (. comp (compare (keyfn x) (keyfn y)))) coll)))

(defn splitv-at
  "Returns a vector of [(into [] (take n) coll) (drop n coll)]
  
  safer/splitv-at additionally is thread-safe for mutable collections
  and generally robust against a mutating coll."
  {:added "1.12"}
  [n coll]
  (let [coll (cond-> coll
               (not (coll? coll)) lazier/sequence)] ;; bind seq if mutable - Ambrose
    [(into [] (take n) coll) (drop n coll)]))

(defn partitionv-all
  "Returns a lazy sequence of vector partitions, but may include
  partitions with fewer than n items at the end.
  Returns a stateful transducer when no collection is provided.
  
  safer/partitionv-all additionally is thread-safe for mutable collections
  and generally robust against a mutating coll."
  {:added "1.12"}
  ([n]
   (partition-all n))
  ([n coll]
   (partitionv-all n n coll))
  ([n step coll]
   (lazy-seq
     (when-let [s (seq coll)]
       (let [seg (into [] (take n) (if (coll? coll) coll s))] ;; use s not coll - Ambrose
         (cons seg (partitionv-all n step (drop step s))))))))

(def 
 ^{:arglists '([coll])
   :doc "Return the last item in coll, in linear time

        safer/last additionally:
        - is thread-safe for mutable collections, at the cost of a call to seq.
        - calls next once every step instead of twice."
   :added "1.0"
   :static true}
 last (fn ^:static last [s]
        (when-let [s (seq s)]   ;; call seq at top and short circuit - Ambrose
          (loop [s s]
            (if-let [n (next s)]
              (recur n) ;; reuse next (might be a bad idea, cache locality?) - Ambrose
              (first s))))))

(def 
 ^{:arglists '([coll])
   :doc "Return a seq of all but the last item in coll, in linear time
        
        safer/butlast additionally:
        - is threadsafe for mutable collections, at the cost of an additional
          call to seq.
        - calls next once per element instead of twice."
   :added "1.0"
   :static true}
 butlast (fn ^:static butlast [s]
           (when-let [s (seq s)] ;; call seq at top and short-circuit - Ambrose
             (loop [ret [] s s]
               (if-let [n (next s)]
                 (recur (conj ret (first s)) n) ;; reuse next - Ambrose
                 (seq ret))))))

#_ ;; prone to races between nth and count, but not obvious how to fix. IndexedSeq comes to mind - Ambrose
(defn rand-nth
  "Return a random element of the (sequential) collection. Will have
  the same performance characteristics as nth for the given
  collection."
  {:added "1.2"
   :static true}
  [coll]
  (nth coll (rand-int (count coll))))
