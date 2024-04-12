(ns io.github.frenchy64.fully-satisfies.cleaners
  "A framework to detect memory leaks via holding the head of sequences.

  The java.lang.ref.Cleaner class (JDK9+) provides hooks into garbage
  collection. You can register a function that is called when a value
  becomes phantom reachable, indicating is it a candidate for garbage
  collection.
  
  The JVM is very likely to perform garbage collection
  right before throwing an OutOfMemoryError. Part of garbage
  collection is calculating whether references are reachable.
  We force a "
  (:require [clojure.test :refer [is]]))

(defmacro ^:private when-jdk9 [& body]
  (when (try (Class/forName "java.lang.ref.Cleaner")
             (catch Throwable _))
    `(do ~@body)))

(when-jdk9 (import [java.lang.ref Cleaner]))

(when-jdk9
  (defn register-cleaner!
    "Register a thunk to be called when object v
    becomes phantom reachable."
    [v f]
    (let [^Runnable f #(f)]
      (.register (Cleaner/create) v f)
      v)))

(when-jdk9
  (defn try-forcing-cleaners!
    "Induce an OutOfMemoryError in an attempt to force Cleaners,
    including those registered by register-cleaner!."
    ([] (try-forcing-cleaners! (constantly false)))
    ([f] (try (loop [c []]
                (System/gc)
                (when-not (f)
                  (recur (conj c (make-array Double/TYPE (dec Integer/MAX_VALUE))))))
              (catch OutOfMemoryError _
                #_(println "OOM")))
     (f))))

(when-jdk9
  (defn head-hold-detecting-lazy-seq
    "Returns a map with entries:
    - :lseq, a lazy sequence where each element is a distinct fresh Object entity, or 
      the result of (i->v <index>) when called.
    - :live, an atom containing a set of indicies that are 
    
    For the most precise results, each element returned by i->v should be distinct according to identical?
    from all other values in the current JVM environment.
    "
    ([] (head-hold-detecting-lazy-seq nil))
    ([{:keys [i->v]
       :or {i->v (fn [i] (Object.))}}]
     (let [live (atom #{})
           rec (fn rec [i]
                 (lazy-seq
                   (let [v (i->v i)]
                     (swap! live conj i)
                     (register-cleaner! v #(swap! live disj i))
                     (concat [v] (rec (inc i))))))]
       {:lseq (rec 0)
        :live live}))))

(when-jdk9
  (defn head-hold-detecting-chunked-seq
    "Each element must be distinct according to identical?."
    ([] (head-hold-detecting-chunked-seq nil))
    ([{:keys [i->v chunk-size]
       :or {i->v (fn [i] (Object.))
            chunk-size 32}}]
     (let [live (atom #{})
           rec (fn rec [i]
                 (lazy-seq
                   (let [b (chunk-buffer chunk-size)]
                     (dotimes [i chunk-size]
                       (let [v (i->v i)]
                         (swap! live conj i)
                         (register-cleaner! v #(swap! live disj i))
                         (chunk-append b v)))
                     (chunk-cons (chunk b) (rec (inc i))))))]
       {:lseq (rec 0)
        :live live}))))

(when-jdk9
  (defn is-live [expected live]
    (loop [tries 100]
      (when (and (pos? tries)
                 (not (try-forcing-cleaners! #(= expected @live))))
        (recur (dec tries))))
    (is (= expected (into (sorted-set) @live)))))
