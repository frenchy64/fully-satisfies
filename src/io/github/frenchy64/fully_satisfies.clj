(ns io.github.frenchy64.fully-satisfies
  (:import [clojure.lang IMeta Var]
           [java.lang.reflect Method Modifier]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn partially-satisfies?
  "Returns true if value v extends protocol p, otherwise false.

  A value is considered to 'extend' protocol p either if:
  - p implements the protocols interface, or
  - p extends the protocol via clojure.core/extend, or
  - p implements at least one method via metadata if supported
    by the protocol"
  [p v]
  (boolean
    (or (find-protocol-impl p v)
        (when-some [vm (and (:extend-via-metadata p)
                            (meta v))]
          (when-some [method-map-keys (-> p :method-map keys seq)]
            (let [^Var pvar (:var p)
                  nstr (-> pvar .ns .name name)]
              (some (fn [mmap-key]
                      (get vm (symbol nstr (name mmap-key))))
                    method-map-keys)))))))

(defn fully-satisfies?
  "Returns true if value v extends protocol p and
  implements every method in protocol p, otherwise false.

  A value is considered to 'extend' protocol p either if:
  - p implements the protocols interface, or
  - p extends the protocol via clojure.core/extend, or
  - p implements at least one method via metadata if supported
    by the protocol"
  [p v]
  (let [c (class v)
        ^Class i (:on-interface p)]
    (if (instance? i v)
      (let [ims (.getMethods i)
            l (alength ims)]
        (loop [idx 0]
          (if (< idx l)
            (let [^Method im (aget ims idx)
                  cm (.getMethod c (.getName im) (.getParameterTypes im))]
              (if (Modifier/isAbstract (.getModifiers cm))
                false
                (recur (unchecked-inc-int idx))))
            true)))
      (let [cimpl (when-some [impls (:impls p)]
                    (or (get impls c)
                        (when (and c (not (identical? Object c)))
                          (let [dfs-for-interface (fn dfs-for-interface [^Class c]
                                                    (when-not (identical? Object c)
                                                      (or (when (.isInterface c)
                                                            (get impls c))
                                                          (some dfs-for-interface (.getInterfaces c))
                                                          (when (not (.isInterface c))
                                                            (recur (.getSuperclass c))))))]
                            (or (loop [^Class c (.getSuperclass c)]
                                  (when-not (identical? Object c)
                                    (or (get impls c)
                                        (recur (.getSuperclass c)))))
                                (dfs-for-interface c)
                                (get impls Object))))))]
        (if cimpl
          (let [ims (.getMethods i)]
            (or (.equals ^Object (count cimpl) (alength ims))
                (if-some [vm (when (:extend-via-metadata p) (meta v))]
                  (let [^Var pvar (:var p)
                        nstr (-> pvar .ns .name name)]
                    (every? (fn [mmap-key]
                              (or (get cimpl mmap-key)
                                  (get vm (symbol nstr (name mmap-key)))))
                            (-> p :method-map keys)))
                  false)))
          (if-some [vm (and (:extend-via-metadata p)
                            (meta v))]
            (if-some [method-map-keys (-> p :method-map keys seq)]
              (let [^Var pvar (:var p)
                    nstr (-> pvar .ns .name name)]
                (every? (fn [mmap-key]
                          (get vm (symbol nstr (name mmap-key))))
                        method-map-keys))
              false)
            false))))))
