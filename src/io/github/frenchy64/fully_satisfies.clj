(ns io.github.frenchy64.fully-satisfies
  (:import [java.lang.reflect Method]))

;; copied from clojure.reflect
(defn- access-flag
  [[name flag & contexts]]
  {:name name :flag flag :contexts (set (map keyword contexts))})

;; copied from clojure.reflect
(def ^{:doc "The Java access bitflags, along with their friendly names and
the kinds of objects to which they can apply."}
  flag-descriptors
  (vec
   (map access-flag
        [[:public 0x0001 :class :field :method]
         [:private 0x002 :class :field :method]
         [:protected 0x0004  :class :field :method]
         [:static 0x0008  :field :method]
         [:final 0x0010  :class :field :method]
         ;; :super is ancient history and is unfindable (?) by
         ;; reflection. skip it
         #_[:super 0x0020  :class]        
         [:synchronized 0x0020  :method]
         [:volatile 0x0040  :field]
         [:bridge 0x0040  :method]
         [:varargs 0x0080  :method]
         [:transient 0x0080  :field]
         [:native 0x0100  :methd]
         [:interface 0x0200  :class]
         [:abstract 0x0400  :class :method]
         [:strict 0x0800  :method]
         [:synthetic 0x1000  :class :field :method]
         [:annotation 0x2000  :class]
         [:enum 0x4000  :class :field :inner]])))

;; copied from clojure.reflect
(defn- parse-flags
  "Convert reflection bitflags into a set of keywords."
  [flags context]
  (reduce
   (fn [result fd]
     (if (and (get (:contexts fd) context)
              (not (zero? (bit-and flags (:flag fd)))))
       (conj result (:name fd))
       result))
   #{}
   flag-descriptors))

(defn- parse-method-flags
  [flags]
  (parse-flags flags :method))

(defn fully-satisfies?
  "Returns true if value v implements every method in protocol p,
  otherwise false."
  [p v]
  (let [c (class v)
        ^Class i (:on-interface p)
        ims (delay (.getMethods i))]
    (boolean
      (or (when (instance? i v)
            (every? (fn [^Method im]
                      (let [cm (.getMethod c (.getName im) (.getParameterTypes im))
                            fs (parse-method-flags (.getModifiers cm))]
                        (not (:abstract fs))))
                    @ims))
          (when-some [impl (or (get-in p [:impls c])
                               (when (not (:extend-via-metadata p))
                                 (get-in p [:impls Object])))]
            (= (count impl)
               (alength @ims)))
          (when (:extend-via-metadata p)
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
                 (alength @ims))))))))
