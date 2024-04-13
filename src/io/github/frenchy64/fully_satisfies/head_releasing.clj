(ns io.github.frenchy64.fully-satisfies.head-releasing
  "Variants of clojure.core functions (and internal helpers)
  that release the head of seqs above all other concerns."
  (:refer-clojure :exclude [map every? not-every?]))

(defn naive-seq-reduce
  "Reduces a seq, ignoring any opportunities to switch to a more
  specialized implementation.
  
  Additionally, head-releasing/naive-seq-reduce does not retain
  a strong reference to the element of s being passed to f while
  calling f."
  [s f val]
  (loop [s (seq s)
         val val]
    (if s
      (let [r (rest s) ;; release head of seq so f can own first
            ret (f val (first s))]
        (if (reduced? ret)
          @ret
          (recur (seq r) ret)))
      val)))

(defn map
  "Returns a lazy sequence consisting of the result of applying f to
  the set of first items of each coll, followed by applying f to the
  set of second items in each coll, until any one of the colls is
  exhausted.  Any remaining items in other colls are ignored. Function
  f should accept number-of-colls arguments. Returns a transducer when
  no collection is provided."
  {:added "1.0"
   :static true}
  ([f]
    (fn [rf]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
           (rf result (f input)))
        ([result input & inputs]
           (rf result (apply f input inputs))))))
  ([f coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (if (chunked-seq? s)
        (let [c (chunk-first s)
              size (int (count c))
              b (chunk-buffer size)]
          (dotimes [i size]
            (chunk-append b (f (.nth c i))))
          (chunk-cons (chunk b) (map f (chunk-rest s))))
        (let [r (rest s)] ;; release a strong reference to (first s) - Ambrose
          (cons (f (first s)) (map f r)))))))
  ([f c1 c2]
   (lazy-seq
     (let [s1 (seq c1) s2 (seq c2)]
       (when (and s1 s2)
         ;; release a strong reference to the first of each seq - Ambrose
         (let [r1 (rest s1) r2 (rest s2)]
           (cons (f (first s1) (first s2))
                 (map f r1 r2)))))))
  ([f c1 c2 c3]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2) s3 (seq c3)]
      (when (and s1 s2 s3)
        ;; release a strong reference to the first of each seq - Ambrose
        (let [r1 (rest s1) r2 (rest s2) r3 (rest s3)]
          (cons (f (first s1) (first s2) (first s3))
                (map f r1 r2 r3)))))))
  ([f c1 c2 c3 & colls]
   (let [step (fn step [cs]
                (lazy-seq
                  (let [ss (map seq cs)]
                    (when (every? identity ss)
                      ;; release a strong reference to the first of each seq - Ambrose
                      (let [rs (doall (map rest ss))]
                        (cons (map first ss) (step rs)))))))]
     (map #(apply f %) (step (conj colls c3 c2 c1))))))

(defn every?
  "Returns true if (pred x) is logical true for every x in coll, else
  false.
  
  head-releasing/every? is additionally:
  - thread-safe for mutable collections
  - does not hold strong reference to coll element
    when passing to pred."
  {:tag Boolean
   :added "1.0"
   :static true}
  [pred coll]
  ;; reuses result of `seq` - Ambrose
  (if-let [coll (seq coll)]
    (let [r (rest coll)] ;; release a strong reference to (first coll) - Ambrose
      (if (pred (first coll))
        (recur pred r)
        false))
    true))

(def
 ^{:tag Boolean
   :doc "Returns false if (pred x) is logical true for every x in
  coll, else true."
   :arglists '([pred coll])
   :added "1.0"}
 not-every? (comp not every?)) ;; use head-releasing/every - Ambrose
