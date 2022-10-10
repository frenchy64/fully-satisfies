(ns io.github.frenchy64.fully-satisfies.cleaners
  (:import [java.lang.ref Cleaner]))

(defn register-cleaner! [v f]
  (let [^Runnable f #(f)]
    (.register (Cleaner/create) v f)
    v))

(defn try-forcing-cleaners!
  ([] (try-forcing-cleaners! {:timeout-ms 10000
                              :growth-multiplier 100000}))
  ([{:keys [timout-ms growth-multiplier]}]
   (let [p (promise)
         o (doto (Object.)
             (register-cleaner! #(deliver p :some-cleaners-ran)))
         v (volatile! (iterate inc' 0))]
     (println "Printing cleaner as evidence" o)
     (try (reduce
            (fn [_ n]
              ;(println (first n))
              (assert (zero? (first @v)) (first @v))
              (if (not= ::timeout (deref p 1 ::timeout))
                (reduced :some-cleaners-ran)
                :unclear-if-cleaners-ran))
            nil
            (partition 1000 @v))
          (catch OutOfMemoryError _
            (vreset! v nil)
            (System/gc)
            (println "Caught OutOfMemoryError!!")
            (println (deref p timout-ms :unclear-if-cleaners-ran))
            (deref p timout-ms :unclear-if-cleaners-ran))))))
