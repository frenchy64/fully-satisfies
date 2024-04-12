(ns io.github.frenchy64.fully-satisfies.head-releasing
  "Variants of clojure.core functions (and internal helpers)
  that release the head of seqs above all other concerns."
  )

;;TODO test
(defn naive-seq-reduce
  "Reduces a seq, ignoring any opportunities to switch to a more
  specialized implementation."
  [s f val]
  (loop [s (seq s)
         val val]
    (if s
      (let [sf (first s)
            s (rest s) ;; release head of seq so f can own sf
            ret (f val sf)]
        (if (reduced? ret)
          @ret
          (recur s ret)))
      val)))
