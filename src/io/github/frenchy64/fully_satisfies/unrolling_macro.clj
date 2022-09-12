(ns io.github.frenchy64.fully-satisfies.unrolling-macro
  "Utilities for unrolling functions."
  (:require [clojure.walk :as walk]))

(defn single-char-syms-from [c]
  {:pre [(<= (int \a) (int c) (int \z))]}
  (concat (map (comp symbol str char)
               (take 26
                     (drop (- (int c) (int \a))
                           (cycle (range (int \a) (inc (int \z)))))))
          (map #(symbol (str c %)) (range))))

(defn gensym-pretty [sym]
  (assert (not (-> sym meta ::original)) sym)
  (assert (symbol? sym) (pr-str sym))
  (with-meta (gensym sym)
             (into (select-keys (meta sym) [:tag])
                   {::original (symbol sym)})))

(defn rest-argv? [argv]
  ;{:pre [(argv? argv)]}
  (= '& (nth argv (- (count argv) 2) nil)))

(defn argv? [argv]
  (and (vector? argv)
       (or (rest-argv? argv)
           (not-any? #{'&} argv))))

(defn argv->fixed-args [argv]
  {:pre [(argv? argv)]
   :post [(every? simple-symbol? %)]}
  (cond-> argv
    (rest-argv? argv) (subvec 0 (- (count argv) 2))))

(defn argv->rest-arg [argv]
  {:pre [(argv? argv)]
   :post [((some-fn nil? simple-symbol?) %)]}
  (when (rest-argv? argv)
    (peek argv)))

(defn prettify-unrolled
  ([v] (prettify-unrolled v {}))
  ([v {:keys [unqualify-core] :as _opts}]
   (walk/prewalk
     (fn [v]
       (or (when (symbol? v)
             (-> v meta ::original))
           (when (and (qualified-symbol? v)
                      (= "clojure.core" (namespace v)))
             (symbol (when-not unqualify-core "cc") (name v)))
           v))
     v)))

(defn gensym-pretty-argvs [argvs]
  {:pre [(every? argv? argvs)]
   :post [(every? argv? %)]}
  (sort (map (fn [argv]
               (let [pretty-rest-arg (some-> (argv->rest-arg argv) gensym-pretty)]
                 (cond-> (mapv gensym-pretty (argv->fixed-args argv))
                   pretty-rest-arg (conj '& pretty-rest-arg))))
             argvs)))

(defn uniformly-flowing-argvs
  "Generates a list argument vectors.

  Use this function if you have a uniform pattern of arities like [], [x], [x y], [x y & args].
  
   :arities        a list of the number of arguments for each arity (add 1 for rest arg).
                   If empty, a single [& rest] arity will be generated. Idiom: (range 0),
                   for (fn [& rest]), (range 1) for (fn ([]) ([& rest])).
   :rest-arity     the number of arguments (fixed + rest) that the rest arity will take.
                   If :skip, no arity will have rest arguments.
                   Default: (if (seq arities) (apply max arities) [1]).
   :fixed-names    a distinct list of variable names to use for fixed arguments
   :rest-name      a name to use for rest argument"
  [{:keys [arities
           rest-arity
           fixed-names
           rest-name]}]
  {:post [(every? argv? %)]}
  (let [[arities rest-arity] (or (when-some [arities (not-empty (vec (sort arities)))]
                                   (if (= :skip rest-arity)
                                     [arities nil]
                                     (or (when (and (not rest-arity)
                                                    (= [0] arities))
                                           [[0 1] 1])
                                         (when rest-arity
                                           [arities rest-arity])
                                         (let [rest-arity (inc (apply max arities))]
                                           [(conj arities rest-arity) rest-arity]))))
                                 (assert (not= :skip rest-arity) "Cannot skip rest arity with empty :arities.")
                                 [[1] 1])]
    (map (fn [nargs]
           (assert (nat-int? nargs) (pr-str nargs))
           (let [rest-arg (when (= nargs rest-arity)
                            (or rest-name 'rest))
                 nfixed (max 0 (cond-> nargs
                                 rest-arg dec))
                 _ (assert (nat-int? nfixed))
                 fixed-args (if fixed-names
                              (into [] (take nfixed) fixed-names)
                              (mapv #(symbol (str "fixed" %)) (range nfixed)))
                 _ (assert (= nfixed (count fixed-args)))]
             (cond-> fixed-args
               rest-arg (conj '& rest-arg))))
         arities)))

(defn unrolled-fn-tail
  ":argvs          a list of argvs of the form [fixed-args*] or [fixed-args* & rest-arg].
                   fixed-args is a list of symbols naming fixed arguments, and rest-arg
                   is a nilable symbol naming a possible rest-argument.
   :this           A symbol to reference the current function. Propagated to first argument of :unrolled-arity.
                   Default: nil
   :unrolled-arity  A 3 argument function taking symbols this, fixed-args, rest-arg,
                    where this and rest-arg are nilable. Returns the body of the arity."
  [{:keys [argvs unrolled-arity this]}]
  (assert (every? argv? argvs) (pr-str argvs this))
  (let [arities (->> (gensym-pretty-argvs argvs)
                     (map (fn [argv]
                            {:pre [(argv? argv)]}
                            (let [fixed-args (argv->fixed-args argv)
                                  rest-arg (argv->rest-arg argv)]
                              (list (cond-> fixed-args
                                      rest-arg (conj '& rest-arg))
                                    (unrolled-arity this fixed-args rest-arg))))))]
    (with-meta (cond-> arities
                 (= 1 (count arities)) first)
               {::argvs argvs})))

;TODO propagate :tag on argv
(defn fn-tail->arglists [fn-tail]
  (map (fn [argv]
         (let [rest-arg (argv->rest-arg argv)]
           (cond-> (argv->fixed-args argv)
             rest-arg (conj '& rest-arg))))
       (-> fn-tail meta ::argvs)))

(defmacro defunrolled [nme doc attr config]
  (assert (symbol? config))
  (let [config-var (or (resolve config)
                       (requiring-resolve
                         (if (simple-symbol? config)
                           (symbol (-> *ns* ns-name name) (name config))
                           config)))
        _ (assert (var? config-var) config)
        fn-tail (unrolled-fn-tail (assoc @config-var :this (symbol (-> *ns* ns-name name) (name nme))))]
    `(defn ~nme ~doc ~(update attr :arglists #(or % (list 'quote (fn-tail->arglists fn-tail))))
       ~@fn-tail)))

;; all from clojure.core, for reference
(comment
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
)
