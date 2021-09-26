(ns io.github.frenchy64.fully-satisfies.never)

(defn never?
  "Returns false for any argument."
  {:tag Boolean}
  [x] false)
