(ns io.github.frenchy64.fully-satisfies.head-releasing
  "Variants of clojure.core functions (and internal helpers)
  that release the head of seqs above all other concerns."
  (:refer-clojure :exclude [map every? not-every? some not-any? mapcat keep]))

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

(defn some
  "Returns the first logical true value of (pred x) for any x in coll,
  else nil.  One common idiom is to use a set as pred, for example
  this will return :fred if :fred is in the sequence, otherwise nil:
  (some #{:fred} coll)"
  {:added "1.0"
   :static true}
  [pred coll]
  (when-let [s (seq coll)]
    (let [r (rest s)] ;; release strong reference to (first s) - Ambrose
      (or (pred (first s))
          (recur pred r)))))

(def
 ^{:tag Boolean
   :doc "Returns false if (pred x) is logical true for any x in coll,
  else true."
   :arglists '([pred coll])
   :added "1.0"}
 not-any? (comp not some)) ;; use head-releasing/some - Ambrose

(defn keep
  "Returns a lazy sequence of the non-nil results of (f item). Note,
  this means false return values will be included.  f must be free of
  side-effects.  Returns a transducer when no collection is provided."
  {:added "1.2"
   :static true}
  ([f]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
          (let [v (f input)]
            (if (nil? v)
              result
              (rf result v)))))))
  ([f coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (if (chunked-seq? s)
        (let [c (chunk-first s)
              size (count c)
              b (chunk-buffer size)]
          (dotimes [i size]
            (let [x (f (.nth c i))]
              (when-not (nil? x)
                (chunk-append b x))))
          (chunk-cons (chunk b) (keep f (chunk-rest s))))
        (let [r (rest s) ;; release strong reference to (first s) - Ambrose
              x (f (first s))]
          (if (nil? x)
            (keep f r)
            (cons x (keep f r)))))))))

#_
(defn mapcat
  "Returns the result of applying concat to the result of applying map
  to f and colls.  Thus function f should return a collection. Returns
  a transducer when no collections are provided"
  {:added "1.0"
   :static true}
  ([f] (comp (map f) cat))
  ([f & colls]
   ;; use head-releasing/map - Ambrose
   (apply concat (apply map f colls))))
