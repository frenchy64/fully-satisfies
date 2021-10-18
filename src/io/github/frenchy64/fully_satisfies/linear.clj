(ns io.github.frenchy64.fully-satisfies.linear
  "Linear-time sequence functions")

(set! *unchecked-math* :warn-on-boxed)

(defn butlast+last
  "Same as (juxt butlast last) for non-empty seqs."
  [s]
  (if-some [s (seq s)]
    (loop [ret (transient [])
           [f & n] s]
      (if n
        (recur (conj! ret f) n)
        [(seq (persistent! ret)) f]))
    (throw (AssertionError. "input must be non-empty"))))

(defn count+last
  "Same as (juxt count last)."
  [s]
  (if (counted? s)
    [(count s) (last s)]
    (if-some [s (seq s)]
      (loop [c 1
             [f & n] s]
        (if n
          (recur (inc c) n)
          [(int c) f]))
      [(int 0) nil])))
