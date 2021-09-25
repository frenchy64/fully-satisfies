(ns io.github.frenchy64.fully-satisfies.each
  "An alternative to `clojure.core/run!` that does not short-circuit on reduced.")

(defn each
  "Serially call f with each subsequent element of c.

  Operationally equivalent to:

    (defn each [f c]
      (reduce #(do (f %2) nil) nil c))"
  [f c]
  (reduce #(do (f %2) nil) nil c))
