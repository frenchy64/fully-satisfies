;;FIXME unit test semantics of all these functions
(ns io.github.frenchy64.fully-satisfies.head-releasing
  "Variants of clojure.core functions (and internal helpers)
  that release the head of seqs earlier, enabling lower peak memory
  usage in some cases.

  For example, many higher-order functions in this namespace release strong
  references to arguments before calling their function arguments.
  Realizing the following example would be prone to failure with an OutOfMemoryError using
  clojure.core/map because map retains a strong reference to takes-75-percent-of-heap
  when calling its function argument.

  (map (fn [takes-75-percent-of-heap]
         (if (< (rand) 0.5)
           takes-75-percent-of-heap
           (generate-another-value-taking-75-percent-of-heap)))
       [takes-75-percent-of-heap])

  In contrast, using head-releasing/map, the garbage collector can reclaim takes-75-percent-of-heap
  while calculating (generate-another-value-taking-75-percent-of-heap) because
  head-releasing/map does not strongly reference takes-75-percent-of-heap at that point.
  
  The basic implementation trick to achieving this is to call (rest s) on the seq currently
  being processed _before_ calling (f (first f)), so the strong reference to (first f)
  is transferred from the higher-order function to f during the call to f."
  (:refer-clojure :exclude [map every? not-every? some not-any? mapcat keep keep-indexed
                            map-indexed]))

(defn naive-seq-reduce
  "Reduces a seq, ignoring any opportunities to switch to a more
  specialized implementation.
  
  Additionally, head-releasing/naive-seq-reduce does not retain
  a strong reference to the arguments of f when it is called."
  [s f val]
  (loop [s (seq s)
         val val]
    (if s
      (let [r (rest s) ;; release head of seq so f can own first - Ambrose
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
  - does not hold strong reference to argument of pred when it is called."
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
  coll, else true.

  Additionally, head-releasing/not-every? does not hold a strong reference
  to the argument of pred when it is called."
   :arglists '([pred coll])
   :added "1.0"}
 not-every? (comp not every?)) ;; use head-releasing/every - Ambrose

(defn map
  "Returns a lazy sequence consisting of the result of applying f to
  the set of first items of each coll, followed by applying f to the
  set of second items in each coll, until any one of the colls is
  exhausted.  Any remaining items in other colls are ignored. Function
  f should accept number-of-colls arguments. Returns a transducer when
  no collection is provided.
  
  Additionally, head-releasing/map does not hold a strong reference
  to the arguments of f when it is called (unless a single chunked seq coll is provided)."
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
  (some #{:fred} coll)
  
  Additionally, head-releasing/some does not hold a strong reference
  to the argument of pred when it is called."
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
  else true.
        
  Additionally, head-releasing/not-any? does not hold a strong reference
  to the argument of pred when it is called."
   :arglists '([pred coll])
   :added "1.0"}
 not-any? (comp not some)) ;; use head-releasing/some - Ambrose

(defn map-indexed
  "Returns a lazy sequence consisting of the result of applying f to 0
  and the first item of coll, followed by applying f to 1 and the second
  item in coll, etc, until coll is exhausted. Thus function f should
  accept 2 arguments, index and item. Returns a stateful transducer when
  no collection is provided.
 
  Additionally, head-releasing/map-indexed does not hold a strong reference
  to the argument of f when it is called (unless a single chunked seq coll is provided)."
  {:added "1.2"
   :static true}
  ([f]
   (fn [rf]
     (let [i (volatile! -1)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (rf result (f (vswap! i inc) input)))))))
  ([f coll]
   (letfn [(mapi [idx coll]
                 (lazy-seq
                   (when-let [s (seq coll)]
                     (if (chunked-seq? s)
                       (let [c (chunk-first s)
                             size (int (count c))
                             b (chunk-buffer size)]
                         (dotimes [i size]
                           (chunk-append b (f (+ idx i) (.nth c i))))
                         (chunk-cons (chunk b) (mapi (+ idx size) (chunk-rest s))))
                       (let [r (rest s)] ;; release strong reference to (first s) - Ambrose
                         (cons (f idx (first s)) (mapi (inc idx) r)))))))]
     (mapi 0 coll))))

(defn keep
  "Returns a lazy sequence of the non-nil results of (f item). Note,
  this means false return values will be included.  f must be free of
  side-effects.  Returns a transducer when no collection is provided.
  
  Additionally, head-releasing/keep does not hold a strong reference
  to the argument of f when it is called (unless a single chunked seq coll is provided)."
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

(defn keep-indexed
  "Returns a lazy sequence of the non-nil results of (f index item). Note,
  this means false return values will be included.  f must be free of
  side-effects.  Returns a stateful transducer when no collection is
  provided.
  
  Additionally, head-releasing/keep-indexed does not hold a strong reference
  to the argument of f when it is called (unless a single chunked seq coll is provided)."
  {:added "1.2"
   :static true}
  ([f]
   (fn [rf]
     (let [iv (volatile! -1)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
            (let [i (vswap! iv inc)
                  v (f i input)]
              (if (nil? v)
                result
                (rf result v))))))))
  ([f coll]
     (letfn [(keepi [idx coll]
               (lazy-seq
                (when-let [s (seq coll)]
                  (if (chunked-seq? s)
                    (let [c (chunk-first s)
                          size (count c)
                          b (chunk-buffer size)]
                      (dotimes [i size]
                        (let [x (f (+ idx i) (.nth c i))]
                          (when-not (nil? x)
                            (chunk-append b x))))
                      (chunk-cons (chunk b) (keepi (+ idx size) (chunk-rest s))))
                    (let [r (rest s) ;; release a strong reference to (first coll) - Ambrose
                          x (f idx (first s))]
                      (if (nil? x)
                        (keepi (inc idx) r)
                        (cons x (keepi (inc idx) r))))))))]
       (keepi 0 coll))))

(defn mapcat
  "Returns the result of applying concat to the result of applying map
  to f and colls.  Thus function f should return a collection. Returns
  a transducer when no collections are provided.
  
  Additionally, head-releasing/mapcat does not hold strong references to
  the arguments of f when it is called (unless a single chunked seq coll is provided)."
  {:added "1.0"
   :static true}
  ([f] (comp (map f) cat))
  ([f & colls]
   ;; use head-releasing/map - Ambrose
   (apply concat (apply map f colls))))
