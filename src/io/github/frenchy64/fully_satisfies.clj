(ns io.github.frenchy64.fully-satisfies
  (:import [java.lang.reflect Method Modifier]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; from https://clojure.atlassian.net/browse/CLJ-2426
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
        (when (:extend-via-metadata p)
          (when-some [vm (not-empty (meta v))]
            (when-some [method-map-keys (-> p :method-map keys seq)]
              (let [nstr (-> p :var symbol namespace)]
                (some (fn [mmap-key]
                        (get vm (symbol nstr (name mmap-key))))
                      method-map-keys))))))))

;; from clojure.core
(defn- super-chain [^Class c]
  (when c
    (cons c (super-chain (.getSuperclass c)))))

;; from clojure.core
(defn- pref
  ([] nil)
  ([a] a) 
  ([^Class a ^Class b]
     (if (.isAssignableFrom a b) b a)))

(defn fully-satisfies?
  "Returns true if value v extends protocol p and
  implements every method in protocol p, otherwise false.

  A value is considered to 'extend' protocol p either if:
  - p implements the protocols interface, or
  - p extends the protocol via clojure.core/extend, or
  - p implements at least one method via metadata if supported
    by the protocol
  
  Note that fully-satisfies? aims to be deterministic even
  in cases where protocol dispatch is non-deterministic, and so
  may be innaccurate in those cases.

  In cases of multiple-inheritance of interfaces, fully-satisfies? will pick
  an implementation by sorting the `supers` of the target object by name
  and finding the first interface (from left to right) that implements
  the protocol. Then, if it finds any more-specific interfaces (ie.,
  sub-interfaces) of this interface in the target class hierarchy, it will pick
  that interface's implementation (and apply this algorithm to a fixed point).

  See also:
  - https://clojure.atlassian.net/browse/CLJ-2656
  - https://clojure.atlassian.net/browse/CLJ-1807"
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
      (if-some [cimpl (when-some [impl (:impls p)]
                        ;; from find-protocol-impl
                        (or (impl c)
                            (when c
                              (or (first (remove nil? (map impl (butlast (super-chain c)))))
                                  ;; use deterministic impl from https://clojure.atlassian.net/browse/CLJ-2656
                                  (when-let [t (reduce pref (sort-by #(.getName ^Class %) (filter impl (disj (supers c) Object))))]
                                    (impl t))
                                  (impl Object)))))]
        (let [ims (.getMethods i)]
          (or (.equals ^Object (count cimpl) (alength ims))
              (if-some [vm (when (:extend-via-metadata p) (meta v))]
                (let [nstr (-> p :var symbol namespace)]
                  (every? (fn [mmap-key]
                            (or (get cimpl mmap-key)
                                (get vm (symbol nstr (name mmap-key)))))
                          (-> p :method-map keys)))
                false)))
        (if-some [vm (and (:extend-via-metadata p)
                          (meta v))]
          (if-some [method-map-keys (-> p :method-map keys seq)]
            (let [nstr (-> p :var symbol namespace)]
              (every? (fn [mmap-key]
                        (get vm (symbol nstr (name mmap-key))))
                      method-map-keys))
            false)
          false)))))
