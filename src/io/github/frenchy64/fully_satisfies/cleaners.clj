(ns io.github.frenchy64.fully-satisfies.cleaners
  (:import [java.lang.ref Cleaner]))

(defn register-cleaner! [v f]
  (let [^Runnable f #(f)]
    (.register (Cleaner/create) v f)
    v))

;; TODO take a function that can short-circuit walking until OOM
;; eg., something that polls the number of cleaners
;; expected to run in a particular test.
(defn try-forcing-cleaners! []
  (try (doall (range))
       (catch OutOfMemoryError _)))
