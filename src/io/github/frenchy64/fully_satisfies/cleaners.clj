(ns io.github.frenchy64.fully-satisfies.cleaners
  (:require [clojure.test :refer [is]]))

(defmacro ^:private when-jdk9 [& body]
  (when (try (Class/forName "java.lang.ref.Cleaner")
             (catch Throwable _))
    `(do ~@body)))

(when-jdk9 (import [java.lang.ref Cleaner]))

(when-jdk9
  (defn register-cleaner! [v f]
    (let [^Runnable f #(f)]
      (.register (Cleaner/create) v f)
      v)))

(when-jdk9
  (defn try-forcing-cleaners!
    ([] (try-forcing-cleaners! (constantly false)))
    ([f] (try (loop [c []]
                (System/gc)
                (when-not (f)
                  (recur (conj c (make-array Double/TYPE (dec Integer/MAX_VALUE))))))
              (catch OutOfMemoryError _
                (println "OOM")))
     nil)))

(when-jdk9
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
                     (register-cleaner! v #(swap! live disj i))
                     (reify clojure.lang.ISeq
                       (first [this] v)
                       (next [this] (seq rst))
                       (more [this] rst)
                       (seq [this] (cons v rst))))))]
       {:lseq (rec 0)
        :live live}))))

(when-jdk9
  (defn is-live [expected live]
    (try-forcing-cleaners! #(= expected @live))
    (is (= expected @live))))
