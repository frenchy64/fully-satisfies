(ns io.github.frenchy64.fully-satisfies
  (:import [clojure.lang Var]
           [java.lang.reflect Method]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn fully-satisfies?
  "Returns true if value v extends protocol p (if applicable) and
  implements every method in protocol p, otherwise false.
  
  Always returns true on :extend-via-metadata protocols with zero methods
  because there is no way to extend them explicitly via metadata and
  all methods are (vacuously) implemented for every value."
  [p v]
  (let [c (class v)
        ^Class i (:on-interface p)
        ims (.getMethods i)]
    (if (instance? i v)
      (let [l (alength ims)]
        (loop [idx 0]
          (if (< idx l)
            (let [^Method im (aget ims idx)
                  cm (.getMethod c (.getName im) (.getParameterTypes im))]
              (if (zero? (bit-and (.getModifiers cm)
                                  ;; abstract flag
                                  0x0400))
                (recur (unchecked-inc-int idx))
                false))
            true)))
      (let [cimpl (when-some [impls (:impls p)]
                    (or (get impls c)
                        (when (and c (not (identical? Object c)))
                          (let [;; copied from clojure.core
                                pref (fn
                                       ([] nil)
                                       ([a] a)
                                       ([^Class a ^Class b]
                                        (if (.isAssignableFrom a b) b a)))]
                            (or (loop [^Class c (.getSuperclass c)]
                                  (when-not (identical? Object c)
                                    (or (get impls c)
                                        (recur (.getSuperclass c)))))
                                (when-some [t (reduce pref
                                                      (filter #(get impls %)
                                                              (disj (supers c) Object)))]
                                  (get impls t))
                                (get impls Object))))))]
        (if cimpl
          (or (.equals ^Object (count cimpl) (alength ims))
              (if-some [vm (when (:extend-via-metadata p) (meta v))]
                (let [^Var pvar (:var p)
                      nstr (-> pvar .ns .name name)]
                  (every? (fn [mmap-key]
                            (or (get cimpl mmap-key)
                                (get vm (symbol nstr (name mmap-key)))))
                          (-> p :method-map keys)))
                false))
          (if (:extend-via-metadata p)
            (let [vm (meta v)
                  ^Var pvar (:var p)
                  nstr (-> pvar .ns .name name)]
              (every? (fn [mmap-key]
                        (get vm (symbol nstr (name mmap-key))))
                      (-> p :method-map keys)))
            false))))))
