(ns io.github.frenchy64.fully-satisfies.unrolling-macro
  "Utilities for unrolling functions."
  (:require [clojure.walk :as walk]))

(defn maybe-apply [f fixed-args rest-args]
  (if rest-args
    `(apply ~f ~@fixed-args ~rest-args)
    (list* f fixed-args)))

(defn maybe-list* [fixed-args rest-args]
  (if (seq fixed-args)
    `(list* ~@fixed-args ~rest-args)
    rest-args))

(defn maybe-concat [& colls]
  (when-some [rests (not-empty (remove nil? colls))]
    (if (next rests)
      `(concat ~@colls)
      (first colls))))

(defn single-char-syms-from [c]
  {:pre [(<= (int \a) (int c) (int \z))]}
  (concat (map (comp symbol str char)
               (take 26
                     (drop (- (int c) (int \a))
                           (cycle (range (int \a) (inc (int \z)))))))
          (map #(symbol (str c %)) (range))))

(defn gensym-pretty [sym]
  (assert (not (-> sym meta ::original)) sym)
  (with-meta (gensym sym) {::original (symbol sym)}))

(defn prettify-unrolled [v]
  (walk/prewalk
    (fn [v]
      (or (when (symbol? v)
            (-> v meta ::original))
          (when (and (qualified-symbol? v)
                     (= "clojure.core" (namespace v)))
            (symbol "cc" (name v)))
          v))
    v))

(defn gensym-pretty-names [names]
  (->> names
       (map (fn [[fixed-args rest-arg]]
              [(mapv gensym-pretty fixed-args) (some-> rest-arg gensym-pretty)]))
       (sort-by (juxt (comp count first) (comp some? second)))))

(defn unrolled-fn-tail*
  ":names          a list of pairs [fixed-args rest-arg] for each arity.
                   fixed-args is a list of symbols naming fixed arguments, and rest-arg
                   is a nilable symbol naming a possible rest-argument.
   :this           A symbol to reference the current function. Propagated to first argument of :unrolled-arity.
                   Default: nil
   :unrolled-arity  A 3 argument function taking symbols this, fixed-args, rest-arg,
                    where this and rest-arg are nilable. Returns the body of the arity."
  [{:keys [names unrolled-arity this]}]
  (let [arities (->> (gensym-pretty-names names)
                     (map (fn [[fixed-args rest-arg]]
                            (list (cond-> fixed-args
                                    rest-arg (conj '& rest-arg))
                                  (unrolled-arity this fixed-args rest-arg)))))]
    (with-meta (cond-> arities
                 (= 1 (count arities)) first)
               {::names names})))

(defn unrolled-fn-tail
  ":arities        a list of the number of arguments for each arity (add 1 for rest arg).
                   If empty, a single [& rest] arity will be generated. Idiom: (range 0),
                   for (fn [& rest]), (range 1) for (fn ([]) ([& rest])).
                   Incompatible with :names.
   :rest-arity     the number of arguments (fixed + rest) that the rest arity will take.
                   If :skip, no arity will have rest arguments.
                   Default: (if (seq arities) (apply max arities) [1]).
                   Incompatible with :names.
   :this           A symbol to reference the current function. Propagated to first argument of :unrolled-arity.
                   Default: nil
   :unrolled-arity  A 3 argument function taking symbols this, fixed-args, rest-arg,
                    where this and rest-arg are nilable. Returns the body of the arity.
   :fixed-names    a distinct list of variable names to use for fixed arguments
                   Incompatible with :names.
   :rest-name      a name to use for rest argument
                   Incompatible with :names.
   :names          a list of pairs [fixed-args rest-arg] for each arity.
                   fixed-args is a list of symbols naming fixed arguments, and rest-arg
                   is a nilable symbol naming a possible rest-argument.
                   Not compatible with :arities, :rest-arity, :fixed-names, or :rest-name."
  [{:keys [this
           arities
           rest-arity
           unrolled-arity
           fixed-names
           rest-name
           names] :as config}]
  (when names
    (assert (not (or arities rest-arity fixed-names rest-name))
            ":names not compatible with :arities, :rest-arity, :fixed-names, or :rest-name"))
  (let [names (or names
                  (let [arities (or (not-empty (sort arities))
                                    (assert (not= :skip rest-arity) "Cannot skip rest arity with empty :arities.")
                                    [1])
                        rest-arity (when (not= :skip rest-arity)
                                     (or rest-arity (apply max arities)))]
                    (map (fn [nargs]
                           (let [rest-arg (when (= nargs rest-arity)
                                            (or rest-name 'rest))
                                 nfixed (cond-> nargs
                                          rest-arg dec)
                                 _ (assert (nat-int? nfixed))
                                 fixed-args (if fixed-names
                                              (into [] (take nfixed) fixed-names)
                                              (mapv #(symbol (str "fixed" %)) (range nfixed)))
                                 _ (assert (= nfixed (count fixed-args)))]
                             [fixed-args rest-arg]))
                         arities)))]
    (unrolled-fn-tail*
      (-> (select-keys config [:this :unrolled-arity])
          (assoc :names names)))))

(defn fn-tail->arglists [fn-tail]
  (map (fn [[fixed-args rest-arg]]
         (cond-> fixed-args
           rest-arg (conj '& rest-arg)))
       (-> fn-tail meta ::names)))

(defmacro defunrolled [nme doc attr config]
  (let [fn-tail (unrolled-fn-tail (-> config resolve deref
                                      (assoc :this (symbol (-> *ns* ns-name name) (name nme)))))]
    `(defn ~nme ~doc ~(update attr :arglists #(or % (list 'quote (fn-tail->arglists fn-tail))))
       ~@fn-tail)))

;; all from clojure.core, for reference
(comment
(defn every-pred
  "Takes a set of predicates and returns a function f that returns true if all of its
  composing predicates return a logical true value against all of its arguments, else it returns
  false. Note that f is short-circuiting in that it will stop execution on the first
  argument that triggers a logical false result against the original predicates."
  {:added "1.3"}
  ([p]
     (fn ep1
       ([] true)
       ([x] (boolean (p x)))
       ([x y] (boolean (and (p x) (p y))))
       ([x y z] (boolean (and (p x) (p y) (p z))))
       ([x y z & args] (boolean (and (ep1 x y z)
                                     (every? p args))))))
  ([p1 p2]
     (fn ep2
       ([] true)
       ([x] (boolean (and (p1 x) (p2 x))))
       ([x y] (boolean (and (p1 x) (p1 y) (p2 x) (p2 y))))
       ([x y z] (boolean (and (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z))))
       ([x y z & args] (boolean (and (ep2 x y z)
                                     (every? #(and (p1 %) (p2 %)) args))))))
  ([p1 p2 p3]
     (fn ep3
       ([] true)
       ([x] (boolean (and (p1 x) (p2 x) (p3 x))))
       ([x y] (boolean (and (p1 x) (p1 y) (p2 x) (p2 y) (p3 x) (p3 y))))
       ([x y z] (boolean (and (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z) (p3 x) (p3 y) (p3 z))))
       ([x y z & args] (boolean (and (ep3 x y z)
                                     (every? #(and (p1 %) (p2 %) (p3 %)) args))))))
  ([p1 p2 p3 & ps]
     (let [ps (list* p1 p2 p3 ps)]
       (fn epn
         ([] true)
         ([x] (every? #(% x) ps))
         ([x y] (every? #(and (% x) (% y)) ps))
         ([x y z] (every? #(and (% x) (% y) (% z)) ps))
         ([x y z & args] (boolean (and (epn x y z)
                                       (every? #(every? % args) ps))))))))


(defn some-fn
  "Takes a set of predicates and returns a function f that returns the first logical true value
  returned by one of its composing predicates against any of its arguments, else it returns
  logical false. Note that f is short-circuiting in that it will stop execution on the first
  argument that triggers a logical true result against the original predicates."
  {:added "1.3"}
  ([p]
     (fn sp1
       ([] nil)
       ([x] (p x))
       ([x y] (or (p x) (p y)))
       ([x y z] (or (p x) (p y) (p z)))
       ([x y z & args] (or (sp1 x y z)
                           (some p args)))))
  ([p1 p2]
     (fn sp2
       ([] nil)
       ([x] (or (p1 x) (p2 x)))
       ([x y] (or (p1 x) (p1 y) (p2 x) (p2 y)))
       ([x y z] (or (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z)))
       ([x y z & args] (or (sp2 x y z)
                           (some #(or (p1 %) (p2 %)) args)))))
  ([p1 p2 p3]
     (fn sp3
       ([] nil)
       ([x] (or (p1 x) (p2 x) (p3 x)))
       ([x y] (or (p1 x) (p1 y) (p2 x) (p2 y) (p3 x) (p3 y)))
       ([x y z] (or (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z) (p3 x) (p3 y) (p3 z)))
       ([x y z & args] (or (sp3 x y z)
                           (some #(or (p1 %) (p2 %) (p3 %)) args)))))
  ([p1 p2 p3 & ps]
     (let [ps (list* p1 p2 p3 ps)]
       (fn spn
         ([] nil)
         ([x] (some #(% x) ps))
         ([x y] (some #(or (% x) (% y)) ps))
         ([x y z] (some #(or (% x) (% y) (% z)) ps))
         ([x y z & args] (or (spn x y z)
                             (some #(some % args) ps)))))))


(defn apply
  "Applies fn f to the argument list formed by prepending intervening arguments to args."
  {:added "1.0"
   :static true}
  ([^clojure.lang.IFn f args]
     (. f (applyTo (seq args))))
  ([^clojure.lang.IFn f x args]
     (. f (applyTo (list* x args))))
  ([^clojure.lang.IFn f x y args]
     (. f (applyTo (list* x y args))))
  ([^clojure.lang.IFn f x y z args]
     (. f (applyTo (list* x y z args))))
  ([^clojure.lang.IFn f a b c d & args]
     (. f (applyTo (cons a (cons b (cons c (cons d (spread args)))))))))
)
