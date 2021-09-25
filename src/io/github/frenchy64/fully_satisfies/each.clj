(ns io.github.frenchy64.fully-satisfies.each)

(defn each
  "(each f e) is operationally equivalent to (reduce #(do (f %2) nil) nil c)."
  [f c]
  (reduce #(do (f %2) nil) nil c))
