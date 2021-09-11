(ns io.github.frenchy64.fully-satisfies
  (:import [java.lang.reflect Method]))

(set! *warn-on-reflection* true)

;; copied from clojure.core
(defn- super-chain [^Class c]
  (when c
    (cons c (super-chain (.getSuperclass c)))))

(defn fully-satisfies?
  "Returns true if value v implements every method in protocol p,
  otherwise false."
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
      (let [impls (:impls p)
            cimpl (or (get impls c)
                      (when c
                        (let [;; copied from clojure.core
                              pref (fn
                                     ([] nil)
                                     ([a] a) 
                                     ([^Class a ^Class b]
                                      (if (.isAssignableFrom a b) b a)))]
                          (or (get impls c)
                              (first
                                (sequence
                                  (mapcat (fn [c]
                                            (when (not (identical? Object c))
                                              (when-some [impl (get impls c)]
                                                [impl]))))
                                  (super-chain c)))
                              (when-some [t (reduce pref
                                                    (filter #(get impls %)
                                                            (disj (supers c) Object)))]
                                (get impls t))
                              (get impls Object)))))]
        (if (:extend-via-metadata p)
          (let [vm (meta v)
                nstr (-> p :var symbol namespace)
                object-impls (get impls Object)]
            (every? (fn [mmap-key]
                      (or (get cimpl mmap-key)
                          (get vm (symbol nstr (name mmap-key)))
                          (get object-impls mmap-key)))
                    (-> p :method-map keys)))
          (if cimpl
            (= (count cimpl)
               (alength ims))
            false))))))
