(ns io.github.frenchy64.fully-satisfies
  (:import [java.lang.reflect Method]))

(set! *warn-on-reflection* true)

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
      (let [evm (:extend-via-metadata p)
            impls (:impls p)]
        (boolean
          (or
            (when-some [impl (or (get impls c)
                                 (when-not evm
                                   (get impls Object)))]
              (= (count impl)
                 (alength ims)))
            (when evm
              (let [nstr (-> p :var symbol namespace)
                    mmap-keys (into #{} (map name) (-> p :method-map keys))
                    impl-keys (into (set (keys (get impls Object)))
                                    (map (fn [[k v]]
                                           (when (and (symbol? k)
                                                      (= nstr (namespace k))
                                                      (contains? mmap-keys (name k)))
                                             (keyword (name k)))))
                                    (meta v))]
                (= (count impl-keys)
                   (alength ims))))))))))
