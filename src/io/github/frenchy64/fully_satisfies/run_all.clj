(ns io.github.frenchy64.fully-satisfies.run-all
  "An alternative to `clojure.core/run!` that does not short-circuit on reduced."
  (:refer-clojure :exclude [run!]))

(defn run-all!
  "Reduce over c and invoke f on each (and every) element. Returns nil."
  [f c]
  (reduce #(do (f %2) nil) nil c))

(def run! run-all!)
