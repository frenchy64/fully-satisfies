(ns io.github.frenchy64.fully-satisfies.expand-kvs)

(defn flatten-trailing-map
  "Given arguments to some function with remaining-fixed number of fixed args,
  return a lazy seq that keeps the fixed args and flattens the last map argument
  if there is an uneven number of remaining (variable) arguments using flatten-fn.
 
  Done lazily in a single pass."
  ([args]
   (flatten-trailing-map 0 args))
  ([remaining-fixed args]
   (flatten-trailing-map #(if (map? %)
                            (apply concat %)
                            (list %))
                         remaining-fixed
                         args))
  ([flatten-fn remaining-fixed args]
   (letfn [(flatten-kvs [even-kvs-so-far? args]
             (lazy-seq
               (when-some [[f & n :as args] (seq args)]
                 (if n
                   (cons f (flatten-kvs (not even-kvs-so-far?) n))
                   (if even-kvs-so-far?
                     (flatten-fn f)
                     args)))))
           (flatten-fixed [remaining-fixed args]
             (lazy-seq
               (when-some [args (seq args)]
                 (cons (first args)
                       (let [remaining-fixed (dec remaining-fixed)]
                         (if (pos? remaining-fixed)
                           (flatten-fixed remaining-fixed (rest args))
                           (flatten-kvs true (rest args))))))))]
     (if (pos? remaining-fixed)
       (flatten-fixed remaining-fixed args)
       (flatten-kvs true args)))))
