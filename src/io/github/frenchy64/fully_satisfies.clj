(ns io.github.frenchy64.fully-satisfies
  (:import [java.lang.reflect Method]))

(defn fully-satisfies?
  "Returns true if value v implements every method in protocol p,
  otherwise false."
  [p v]
  (let [c (class v)
        ^Class i (:on-interface p)
        ims (delay (.getMethods i))]
    (if (instance? i v)
      (every? (fn [^Method im]
                (let [cm (.getMethod c (.getName im) (.getParameterTypes im))]
                  (zero? (bit-and (.getModifiers cm)
                                  ;; abstract flag
                                  0x0400))))
              @ims)
      (let [evm (:extend-via-metadata p)]
        (boolean
          (or
            (when-some [impl (or (get-in p [:impls c])
                                 (when-not evm
                                   (get-in p [:impls Object])))]
              (= (count impl)
                 (alength @ims)))
            (when evm
              (let [nstr (-> p :var symbol namespace)
                    mmap-keys (into #{} (map name) (-> p :method-map keys))
                    impl-keys (into (set (keys (get-in p [:impls Object])))
                                    (map (fn [[k v]]
                                           (when (and (symbol? k)
                                                      (= nstr (namespace k))
                                                      (contains? mmap-keys (name k)))
                                             (keyword (name k)))))
                                    (meta v))]
                (= (count impl-keys)
                   (alength @ims))))))))))
