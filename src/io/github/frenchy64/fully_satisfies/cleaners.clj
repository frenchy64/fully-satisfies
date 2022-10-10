(ns io.github.frenchy64.fully-satisfies.cleaners
  (:import [java.lang.ref Cleaner]))

(defn register-cleaner! [v f]
  (let [^Runnable f #(f)]
    (.register (Cleaner/create) v f)
    v))

(defn try-forcing-cleaners!
  ([] (try-forcing-cleaners! (constantly false)))
  ([f] (let [c (volatile! (range))]
         (try (loop [c @c]
                (when-not (f)
                  (prn "not" (first c))
                  (System/gc)
                  (recur (nthnext c 100000))))
              (catch OutOfMemoryError _
                (vreset! c nil)
                (System/gc)
                (println "OOM")))
         (first @c)
         nil)))

(defn head-hold-detecting-lazy-seq
  "Each element must be distinct according to identical?."
  ([] (head-hold-detecting-lazy-seq (fn [i] (Object.))))
  ([i->v]
   (let [live (atom #{})
         rec (fn rec [i]
               (lazy-seq
                 (let [v (i->v i)
                       rst (rec (inc i))]
                   (swap! live conj i)
                   (prn live)
                   (register-cleaner! v #(swap! live disj i))
                   (reify clojure.lang.ISeq
                     (first [this] v)
                     (next [this] (seq rst))
                     (more [this] rst)
                     (seq [this] (cons v rst))))))]
     {:lseq (rec 0)
      :live live})))
