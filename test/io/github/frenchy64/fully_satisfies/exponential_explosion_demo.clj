(ns io.github.frenchy64.fully-satisfies.exponential-explosion-demo)

#_
(comment
  (require (ns-name *ns*) :reload))

(defmacro rebind-info [m & body]
  `(with-bindings
     (into {} (map (fn [[k# v#]]
                     {(case k#
                        :file #'*file
                        :line #'clojure.lang.Compiler/LINE
                        :column #'clojure.lang.Compiler/COLUMN)
                      v#}))
           ~m)
     (do ~@body)))

(defmacro duplicated [x]
  #_
  (prn 'duplicated
       {:file *file*
        :line (.deref clojure.lang.Compiler/LINE)
        :column (.deref clojure.lang.Compiler/COLUMN)})
  x)

(defmacro exponential [x]
  #_
  (prn 'exponential
       {:file *file*
        :line (.deref clojure.lang.Compiler/LINE)
        :column (.deref clojure.lang.Compiler/COLUMN)})
  `(do ~x ~x))

(#_comment
 do
  (duplicated 1)
  (duplicated (let []))
  (exponential (duplicated 1))
  (exponential (let [] (duplicated 1)))
  (exponential (exponential (duplicated 1)))
  (doseq [_ nil] (exponential (let [] (duplicated 1))))
  (doseq [_ nil] (duplicated 1))
  (do #(doseq [_ (duplicated 1)] 1))
  (do #(doseq [_ (duplicated 1) _ (duplicated 1)] 1))
  (do #(doseq [_ (duplicated 1) _ (duplicated 1)] (duplicated 1)))
  (io.github.frenchy64.fully-satisfies.linear-expansion/doseq [_ nil] (duplicated 1))
  (exponential (doseq [_ nil] (let [] (duplicated 1))))
  (let [] (exponential (duplicated 1)))
  (exponential (do (-> 1 duplicated)
                   (-> 2 duplicated exponential)))
  (exponential
    (exponential
      (exponential
        (do (-> 1 duplicated)
            (-> 2 duplicated)))))
  )
