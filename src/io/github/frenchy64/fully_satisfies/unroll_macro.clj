(ns io.github.frenchy64.fully-satisfies.unroll-macro
  "Utilities to unroll functions."
  (:require [clojure.walk :as walk]))

(defn single-char-syms-from [c]
  {:pre [(<= (int \a) (int c) (int \z))]}
  (concat (map (comp symbol str char)
               (take 26
                     (drop (- (int c) (int \a))
                           (cycle (range (int \a) (inc (int \z)))))))
          (map #(symbol (str c %)) (range))))

(defn gensym-pretty [sym]
  (let [sym (symbol sym)]
    (assert (not (-> sym meta ::original)) sym)
    (with-meta (gensym sym)
               (into (select-keys (meta sym) [:tag])
                     {::original sym}))))

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

(defn prettify-unroll
  ([v] (prettify-unroll v {}))
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

;;FIXME add :leading-fixed-names so the (range 0) idiom can be used with
;; leading args. eg., swap!/swap-vals!
(defn uniformly-flowing-argvs
  "Generates a list of argument vectors.

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

(defn flatten-arities [arities]
  (cond-> arities
    (= 1 (count arities)) (-> first
                              (with-meta (meta arities)))))

(defn unroll-arities
  "Returns a sequence of arities as allowed by fn or defn.

   :argvs          a list of argvs of the form [fixed-args*] or [fixed-args* & rest-arg].
                   fixed-args is a list of symbols naming fixed arguments, and rest-arg
                   is a nilable symbol naming a possible rest-argument.
   :this           A symbol to reference the current function. Propagated to first argument of :unroll-arity.
                   Default: nil
   :unroll-arity  A 3 argument function taking symbols this, fixed-args, rest-arg,
                    where this and rest-arg are nilable. Returns the body of the arity."
  [{:keys [argvs unroll-arity this]}]
  (assert (every? argv? argvs) (pr-str argvs this))
  (let [gargvs (gensym-pretty-argvs argvs)
        arities (map (fn [argv]
                       {:pre [(argv? argv)]}
                       (list argv
                             (unroll-arity {:this this
                                            :argvs gargvs
                                            :argv argv
                                            :fixed-args (argv->fixed-args argv)
                                            :rest-arg (argv->rest-arg argv)})))
                     gargvs)]
    (flatten-arities
      (with-meta arities {::argvs argvs}))))

(defmacro defunroll [nme doc attr config]
  (assert (symbol? config))
  (let [config-var (or (resolve config)
                       (requiring-resolve
                         (if (simple-symbol? config)
                           (symbol (-> *ns* ns-name name) (name config))
                           config)))
        _ (assert (var? config-var) config)
        {:keys [argvs] :as c} (assoc @config-var :this (symbol (-> *ns* ns-name name) (name nme)))]
    `(defn ~nme ~doc ~(update attr :arglists #(or % (list 'quote argvs)))
       ~@(unroll-arities c))))
