(ns io.github.frenchy64.fully-satisfies.expand-kvs)

(defn expand-kvs-seq
  "Given arguments to a function with remaining-fixed number of fixed args
  and kw arguments with an supported trailing map, return a lazy seq with
  trailing maps expanded in a single (lazy) pass."
  ([remaining-fixed args]
   (expand-kvs-seq #(if (map? %)
                      (apply concat %)
                      (list %))
                   remaining-fixed
                   args))
  ([expand-fn remaining-fixed args]
   (letfn [(expand-kvs-seq [even-kvs-so-far? args]
             (lazy-seq
               (when-let [[f & n] (seq args)]
                 (if n
                   (cons f (expand-kvs-seq (not even-kvs-so-far?) n))
                   (if even-kvs-so-far?
                     (expand-fn f)
                     args)))))
           (expand-fixed-seq [remaining-fixed args]
             (lazy-seq
               (when (seq args)
                 (cons (first args)
                       (let [remaining-fixed (dec remaining-fixed)]
                         (if (pos? remaining-fixed)
                           (expand-fixed-seq remaining-fixed (rest args))
                           (expand-kvs-seq true (rest args))))))))]
     (if (pos? remaining-fixed)
       (expand-fixed-seq remaining-fixed args)
       (expand-kvs-seq true args)))))
