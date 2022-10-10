(ns io.github.frenchy64.fully-satisfies.cleaners
  (:import [java.lang.ref Cleaner]))

(defn register-cleaner! [v f]
  (let [^Runnable f #(f)]
    (.register (Cleaner/create) v f)
    v))

(defn try-forcing-cleaners! []
  (try (doall (range))
       (catch OutOfMemoryError _)))
