(ns io.github.frenchy64.fully-satisfies.head-releasing
  "Variants of clojure.core functions (and internal helpers)
  that release the head of seqs above all other concerns.")

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
