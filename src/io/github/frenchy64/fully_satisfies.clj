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
      (let [_ (sort-by #(.getName ^Method %) ims)
            cms (.getMethods c)
            _ (sort-by #(.getName ^Method %) cms)
            l (alength ims)]
        (loop [ims-idx 0
               cms-idx 0]
          (if (< ims-idx l)
            (let [^Method im (aget ims ims-idx)
                  cms-idx-volatile (volatile! nil)
                  ^Method cm (loop [cms-idx cms-idx]
                               (let [^Method cm (aget cms cms-idx)]
                                 (if (= (.getName im)
                                        (.getName cm))
                                   (do (vreset! cms-idx-volatile cms-idx)
                                       cm)
                                   (recur (unchecked-inc-int cms-idx)))))]
              (if (zero? (bit-and (.getModifiers cm)
                                  ;; abstract flag
                                  0x0400))
                (recur (unchecked-inc-int ims-idx)
                       (unchecked-inc-int @cms-idx-volatile))
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
