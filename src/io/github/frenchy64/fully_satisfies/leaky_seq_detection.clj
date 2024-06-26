(ns io.github.frenchy64.fully-satisfies.leaky-seq-detection
  "A framework to detect memory leaks caused by holding onto the head of sequences.

  The java.lang.ref.Cleaner class (JDK9+) provides hooks into garbage
  collection. You can register a function that is called when a value
  becomes phantom reachable, indicating is it a candidate for garbage
  collection.
  
  The JVM is very likely to perform garbage collection
  right before throwing an OutOfMemoryError. Part of garbage
  collection is calculating whether references are reachable.
  We use this insight to force garbage collection (and hence, cleaners)
  to run, by inducing an OutOfMemoryError in try-forcing-cleaners!.

  Note that an OutOfMemoryError can leave the JVM in a bad state, so
  this strategy is best isolated away from other tests. It is unclear
  whether this helps mitigate any such issues or has any affect whatsoever,
  but try-forcing-cleaners! requests large blocks of memory at a time
  in the hope that when the JVM does refuse to allocate more memory,
  there is a better chance that sufficient memory will be left over for
  normal execution. Suggestions welcome for better strategies.
  
  Tying these ideas together are reference-counting seqs and the is-strong
  testing macro. ref-counting-lazy-seq returns a lazy seq
  and an atom of all elements of the seq currently with strong references.
  This seq can now be passed to a sequence-processing function you would
  like to test for memory leaks.

  is-strong then takes a set of seq indexes expected to have strong references
  and checks them against the atom tracking strong references. It will continue
  to induce OutOfMemoryError's until the expected references are found, or eventually
  fail via an is test assertion.
 
  Note that it's best to build up the set of expected strong references rather than
  whittle it down. From a usability standpoint, starting with (is-strong #{} live)
  will likely print the actual set of strongly referenced indexes, from which you can
  source the expected strong references (from the result itself or a subset).
  But false-positives are possible if an overly broad superset of the actual strong references are provided,
  since is-strong may short-circuit its search earlier than the cleaners can run.
  Such results can lead to false conclusions about how lazy a function actually is, especially
  if you have a pre-conceived notion of the results! The JVM property
    io.github.frenchy64.fully-satisfies.leaky-seq-detection.is-strong.false-positive-detection=true
  will more aggressively search for such false-positives, which may be suitable for a cron CI job.

  Here's an example of asserting that a program adds or subtracts strong references to elements
  of a lazy seq at particular points.
  
  (deftest example-cleaners-test
    (let [{:keys [strong lseq]} (ref-counting-lazy-seq
                                  {:n 10}) ;; seq of fresh Object's, length 10
          ;; lseq=(...)
          _ (is-strong #{} strong) ;; no elements currently in memory
          lseq (seq lseq)
          ;; lseq=(0 ...)
          _ (is-strong #{0} strong) ;; just the first element in memory
          _ (nnext lseq)
          ;; lseq=(0 1 2...)
          _ (is-strong #{0 1 2} strong) ;; the first 3 elements in memory
          lseq (next lseq)
          ;; lseq=(1 2 ...)
          _ (is-strong #{1 2} strong) ;; the second 2 elements in memory
          lseq (rest lseq)
          ;; lseq=(2 ...)
          _ (is-strong #{2} strong) ;; the third element in memory
          lseq (rest lseq)
          ;; lseq=(...)
          _ (is-strong #{} strong) ;; no elements in memory
          ;; lseq=(3 ...)
          lseq (seq lseq)
          _ (is-strong #{3} strong) ;; fourth element in memory
          _ (class (first lseq)) ;; add a strong reference to lseq so previous line succeeds
          ;; lseq=nil
          _ (is-strong #{} strong) ;; lseq is entirely garbage collected
          ]))
  
  See io.github.frenchy64.fully-satisfies.leaky-seq-detection-test for real-world
  examples of finding memory leaks in Clojure functions, and then verifying fixes for them."
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]))

(defmacro ^:private when-jdk9 [& body]
  (when (try (Class/forName "java.lang.ref.Cleaner")
             (catch Throwable _))
    `(do ~@body)))

(when-jdk9 (import [java.lang.ref Cleaner]))

(when-jdk9
  (defonce ^:private -cleaner (Cleaner/create))
  (defn register-cleaner!
    "Register a thunk to be called when object v
    becomes phantom reachable."
    [v f]
    (let [^Runnable f #(f)]
      (.register -cleaner v f)
      v)))

(when-jdk9
  (defn try-forcing-cleaners!
    "Induce an OutOfMemoryError in an attempt to force Cleaners,
    including those registered by register-cleaner!."
    ([] (try-forcing-cleaners! (constantly false)))
    ([f] (try (loop [c []]
                (System/gc)
                (when-not (f)
                  ;; https://stackoverflow.com/questions/3038392/do-java-arrays-have-a-maximum-size
                  (recur (conj c (make-array Object (- Integer/MAX_VALUE 100))))))
              (catch OutOfMemoryError e
                (let [msg (ex-message e)]
                  ;; if this is too specific, we at least don't want: Requested array size exceeds VM limit.
                  ;; seems unlikely to force garbage collection.
                  (assert (= "Java heap space" msg)
                          msg))
                #_(println "OOM")))
     (f))))

(when-jdk9
  (defn ref-counting-lazy-seq
    "Returns a map with entries:
    - :lseq, an lazy sequence of length n (default ##Inf) where each element is a distinct fresh Object entity, or 
      the result of (i->v {:i <index> :end <eos>}) when provided. Returning :end key from i->v arg also ends the sequence.
    - :strong, an atom containing a set of indicies whose values are (likely) currently strong references.
    
    For the most precise results, each element returned by i->v should be distinct according to identical?
    from all other values in the current JVM environment."
    ([] (ref-counting-lazy-seq nil))
    ([{:keys [i->v n]
       :or {i->v (fn [{:keys [i end]}] (Object.))
            n ##Inf}}]
     (let [strong (atom (sorted-set))
           rec (fn rec [i]
                 (lazy-seq
                   (when (< i n)
                     (let [end (Object.)
                           v (i->v {:i i :end end})]
                       (when-not (identical? v end)
                         (swap! strong conj i)
                         (register-cleaner! v #(swap! strong disj i))
                         (cons v (rec (inc i))))))))]
       {:lseq (rec 0)
        :strong strong}))))

(when-jdk9
  (defn ref-counting-chunked-seq
    "Returns a map with entries:
    - :lseq, a lazily chunked sequence of length n (default ##Inf) where each element is a distinct fresh Object entity, or 
      the result of (i->v {:i <index> :end eos}) when provided. Chunks are of size chunk-size (default 32).
      Returning :end key from i->v arg also ends the sequence.
    - :strong, an atom containing a set of indicies whose values are (likely) currently strong references.
    
    For the most precise results, each element returned by i->v should be distinct according to identical?
    from all other values in the current JVM environment."
    ([] (ref-counting-chunked-seq nil))
    ([{:keys [i->v chunk-size n]
       :or {i->v (fn [{:keys [i end]}] (Object.))
            n ##Inf
            chunk-size 32}}]
     (let [strong (atom (sorted-set))
           rec (fn rec [i]
                 (lazy-seq
                   (when (< i n)
                     (let [chunk-size (cond-> chunk-size
                                        (not= ##Inf n)
                                        (min (- n i)))
                           b (chunk-buffer chunk-size)]
                       (run! (fn [i]
                               (let [end (Object.)
                                     v (i->v {:i i :end end})]
                                 (if (identical? v end)
                                   (reduced nil)
                                   (do (swap! strong conj i)
                                       (register-cleaner! v #(swap! strong disj i))
                                       (chunk-append b v)))))
                             (range i (+ i chunk-size)))
                       (chunk-cons (chunk b) (rec (+ i chunk-size)))))))]
       {:lseq (rec 0)
        :strong strong}))))

(when-jdk9
  (defn- -is-strong-msg [expected actual]
    (assert (sorted? expected))
    (assert (sorted? actual))
    (let [missing (not-empty (apply disj expected actual))
          extra (not-empty (apply disj actual expected))]
      (not-empty
        (str/join "\n"
                  (cond-> []
                    missing
                    (conj (str "Missing strong references to indexes: "
                               (pr-str missing)))
                    extra
                    (conj (str "Unexpected strong references to indexes: "
                               (pr-str extra)))))))))

(when-jdk9
  (defn is-strong
    "Asserts that there are strong references to the expected set of indexes
    into the ref-counting seq associated with strong by yielding OutOfMemoryError's
    via [[try-forcing-cleaners!]].
    
    Short-circuits if expected set equals actual set.

    Setting the system property:
      io.github.frenchy64.fully-satisfies.leaky-seq-detection.is-strong.false-positive-detection=true
    will prevent short-circuiting in order to detect false-positives (see also leaky-seq-detection's docstring)."
    [expected strong]
    (let [f (if (= "true"
                   (System/getProperty
                     "io.github.frenchy64.fully-satisfies.leaky-seq-detection.is-strong.false-positive-detection"))
              #(do false)
              #(= expected @strong))]
      (loop [tries 100]
        (when (and (pos? tries)
                   (not (try-forcing-cleaners! f)))
          (recur (dec tries)))))
    (let [expected (into (sorted-set) expected)
          actual @strong]
      (is (= expected actual)
          (-is-strong-msg expected actual)))))
