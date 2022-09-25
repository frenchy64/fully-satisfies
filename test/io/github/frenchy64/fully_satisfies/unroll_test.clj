(ns io.github.frenchy64.fully-satisfies.unroll-test
  (:require [clojure.core :as cc]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [clojure.test :refer [is]]
            [clojure.math.combinatorics :as comb]
            [io.github.frenchy64.fully-satisfies.unroll
             :refer [defunroll unroll-arities gensym-pretty prettify-unroll
                     argv->fixed-args argv->rest-arg single-char-syms-from flatten-arities uniformly-flowing-argvs]]))

(defn maybe-apply [f fixed-args rest-args]
  (assert f)
  (if (some? rest-args)
    `(apply ~f ~@fixed-args ~rest-args)
    (list* f fixed-args)))

(defn maybe-list* [fixed-args rest-args]
  (assert ((some-fn nil? symbol?) rest-args))
  (if (seq fixed-args)
    (if rest-args
      `(list* ~@fixed-args ~rest-args)
      `(list ~@fixed-args))
    (or rest-args ())))

(defn maybe-when [tst then]
  (when (some? tst)
    `(when ~tst ~then)))

(deftest maybe-list*-test
  (is (= 'a (maybe-list* nil 'a)))
  (is (= `(list* ~'b ~'a) (maybe-list* ['b] 'a)))
  (is (= `(list ~'b) (maybe-list* ['b] nil)))
  (is (= () (maybe-list* nil nil))))

(defn maybe-concat [& colls]
  (when-some [rests (not-empty (remove nil? colls))]
    (if (next rests)
      `(concat ~@rests)
      (first rests))))

(deftest maybe-concat-test
  (is (= nil (maybe-concat)))
  (is (= 'a (maybe-concat 'a)))
  (is (= `(concat ~'a ~'b) (maybe-concat 'a 'b)))
  (is (= 'b (maybe-concat nil 'b)))
  (is (= `(concat ~'a ~'b ~'c) (maybe-concat 'a 'b 'c)))
  (is (= `(concat ~'a ~'c) (maybe-concat 'a nil 'c)))
  (is (= 'a (maybe-concat 'a nil nil)))
  (is (= nil (maybe-concat nil nil nil))))

(defn maybe-and [exprs]
  (let [exprs (remove true? exprs)]
    (case (count exprs)
      0 true
      1 (first exprs)
      `(and ~@exprs))))

(defn maybe-reduce [f init coll]
  (if (nil? coll)
    init
    `(reduce ~f ~init ~coll)))

(defn maybe-> [init es]
  (if (empty? es)
    init
    `(-> ~init ~@es)))

(defn non-false-expr?
  "Expression will never return the value `false`."
  [e]
  (or ((some-fn nil? number? true?) e)
      (and (seq? e)
           (#{`some} (first e))
           (= 3 (count e)))))

(defn maybe-or-nil [exprs]
  (let [exprs (remove nil? exprs)]
    (if (= 0 (count exprs))
      nil
      (if (and (= 1 (count exprs))
               (non-false-expr? (first exprs)))
        (first exprs)
        `(or ~@exprs ~@(when-not (non-false-expr? (last exprs))
                         [nil]))))))

(deftest maybe-or-nil-test
  (is (= nil (maybe-or-nil [])))
  (is (= `(some ~'f ~'coll) (maybe-or-nil [`(some ~'f ~'coll) nil])))
  (is (= `(or ~'f ~'g nil) (maybe-or-nil ['f 'g]))))

(defn maybe-or [exprs]
  (case (count exprs)
    0 nil
    1 (first exprs)
    `(or ~@exprs)))

(defn maybe-conj [target xs]
  (if (seq xs)
    `(conj ~target ~@xs)
    target))

(defn true-expression? [e]
  (true? e))

(defn false-expression? [e]
  (or (nil? e)
      (false? e)))

(defn single-arg-function-returning? [coll f]
  (boolean (when (seq? coll)
             (when (and (= 3 (count coll))
                        (= `fn (first coll))
                        (vector? (second coll))
                        (= 1 (count (second coll))))
               (f (last coll))))))

(deftest single-arg-true-function?-test
  (is (single-arg-function-returning? `(fn [_] true) true-expression?))
  (is (not (single-arg-function-returning? `(fn [] true) true-expression?)))
  (is (not (single-arg-function-returning? `(fn [_] false) true-expression?)))
  (is (single-arg-function-returning? `(fn [_] false) false-expression?)))

(defn maybe-every? [f coll]
  (if (single-arg-function-returning? f true-expression?)
    true
    `(every? ~f ~coll)))

(deftest maybe-every?-test
  (is (= true (maybe-every? `(fn [_] true) 'coll)))
  (is (= `(every? (fn [~'_ ~'_] true) ~'coll) (maybe-every? `(fn [~'_ ~'_] true) 'coll)))
  (is (= `(every? (fn [~'p] ~'p) ~'coll) (maybe-every? `(fn [~'p] ~'p) 'coll))))

(defn maybe-some [f coll]
  (when-not (single-arg-function-returning? f false-expression?)
    `(some ~f ~coll)))

(deftest maybe-some-test
  (is (= nil (maybe-some `(fn [_] nil) 'coll)))
  (is (= `(some (fn [~'_ ~'_] nil) ~'coll) (maybe-some `(fn [~'_ ~'_] nil) 'coll)))
  (is (= `(some (fn [~'p] ~'p) ~'coll) (maybe-some `(fn [~'p] ~'p) 'coll))))

(defn boolean-function? [sym]
  (boolean
    (when (qualified-symbol? sym)
      (let [v (resolve sym)]
        (when (var? v)
          (#{Boolean} (-> v meta :tag)))))))

(deftest boolean-function?-test
  (is (boolean-function? `every?))
  (is (not (boolean-function? `some))))

(defn boolean-expr? [expr]
  (boolean (or (boolean? expr)
               (when (seq? expr)
                 (boolean-function? (first expr)))
               (and (seq? expr)
                    (-> expr first #{`and})
                    (every? boolean? (rest expr))))))

(defn maybe-boolean [expr]
  (cond->> expr
    (not (boolean-expr? expr)) (list `boolean)))

(deftest uniformly-flowing-argvs-test
  (is (= (uniformly-flowing-argvs {:rest-name 'rest})
         '([& rest])))
  (is (= (map #(uniformly-flowing-argvs
                 {:arities (range %)
                  :fixed-names (single-char-syms-from \a)
                  :rest-name 'args})
              (range 6))
         '(([& args])
           ([] [& args])
           ([] [a] [a & args])
           ([] [a] [a b] [a b & args])
           ([] [a] [a b] [a b c] [a b c & args])
           ([] [a] [a b] [a b c] [a b c d] [a b c d & args])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/vector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
(defn vector
  "Creates a new vector containing the args."
  {:added "1.0"
   :static true}
  ([] [])
  ([a] [a])
  ([a b] [a b])
  ([a b c] [a b c])
  ([a b c d] [a b c d])
	([a b c d e] [a b c d e])
	([a b c d e f] [a b c d e f])
  ([a b c d e f & args]
     (. clojure.lang.LazilyPersistentVector (create (cons a (cons b (cons c (cons d (cons e (cons f args))))))))))

(defn unroll-vector-spec*
  ([] (unroll-vector-spec* {}))
  ([{:keys [size] :or {size 7}}]
   {:argvs (uniformly-flowing-argvs
             {:arities (range size)
              :fixed-names (single-char-syms-from \a)
              :rest-name 'args})
    :unroll-arity (fn [{:keys [fixed-args rest-arg]}]
                    (if rest-arg
                      `(clojure.lang.LazilyPersistentVector/create
                         ~(reduce (fn [acc x] `(cons ~x ~acc)) rest-arg (rseq fixed-args)))
                      fixed-args))}))

(def unroll-vector-spec (unroll-vector-spec*))

(deftest unroll-vector-spec-test
  (is (= (prettify-unroll (unroll-arities (unroll-vector-spec* {:size 0})))
         '([& args] (clojure.lang.LazilyPersistentVector/create args))))
  (is (= (prettify-unroll (unroll-arities (unroll-vector-spec* {:size 1})))
         '(([] [])
           ([& args] (clojure.lang.LazilyPersistentVector/create args)))))
  (is (= (prettify-unroll (unroll-arities (unroll-vector-spec* {:size 2})))
         '(([] [])
           ([a] [a])
           ([a & args] (clojure.lang.LazilyPersistentVector/create (cc/cons a args))))))
  (is (= (prettify-unroll (unroll-arities unroll-vector-spec))
         '(([] [])
           ([a] [a])
           ([a b] [a b])
           ([a b c] [a b c])
           ([a b c d] [a b c d])
           ([a b c d e] [a b c d e])
           ([a b c d e f] [a b c d e f])
           ([a b c d e f & args] (clojure.lang.LazilyPersistentVector/create (cc/cons a (cc/cons b (cc/cons c (cc/cons d (cc/cons e (cc/cons f args))))))))))))

(defunroll unroll-vector
  "Creates a new vector containing the args."
  {:added "1.0"
   :static true}
  unroll-vector-spec)

(deftest unroll-vector-test
  (is (= (-> #'unroll-vector meta :arglists)
         (-> #'clojure.core/vector meta :arglists)
         '([] [a] [a b] [a b c] [a b c d] [a b c d e] [a b c d e f] [a b c d e f & args])))
  (dotimes [i 10]
    (is (= (apply clojure.core/vector (range i))
           (apply unroll-vector (range i))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
(defn list
  ([] clojure.lang.PersistentList/EMPTY)
  ([a] (-> clojure.lang.PersistentList/EMPTY (conj a)))
  ([a b] (-> clojure.lang.PersistentList/EMPTY (conj a) (conj b)))
  ([a b & args] (apply clojure.lang.PersistentList/creator a b args)))

(def unroll-list-spec
  {:argvs (uniformly-flowing-argvs
            {:arities (range 5)
             :fixed-names (single-char-syms-from \a)
             :rest-name 'args})
   :unroll-arity (fn [{:keys [fixed-args rest-arg]}]
                   (if rest-arg
                     `(apply clojure.lang.PersistentList/creator ~@fixed-args ~rest-arg)
                     `(-> clojure.lang.PersistentList/EMPTY
                          ~@(map (fn [a] `(conj ~a))
                                 (rseq fixed-args)))))})

(deftest unroll-list-spec-test
  (is (= (prettify-unroll (unroll-arities unroll-list-spec))
         '(([] (cc/-> clojure.lang.PersistentList/EMPTY))
           ([a] (cc/-> clojure.lang.PersistentList/EMPTY (cc/conj a)))
           ([a b] (cc/-> clojure.lang.PersistentList/EMPTY (cc/conj b) (cc/conj a)))
           ([a b c] (cc/-> clojure.lang.PersistentList/EMPTY (cc/conj c) (cc/conj b) (cc/conj a)))
           ([a b c d] (cc/-> clojure.lang.PersistentList/EMPTY (cc/conj d) (cc/conj c) (cc/conj b) (cc/conj a)))
           ([a b c d & args] (cc/apply clojure.lang.PersistentList/creator a b c d args))))))

(defunroll unroll-list
  "doc"
  {}
  unroll-list-spec)

(deftest unroll-list-test
  (dotimes [i 10]
    (is (= (apply clojure.core/list (range i))
           (apply unroll-list (range i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/list*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
(defn list*
  "Creates a new seq containing the items prepended to the rest, the
  last of which will be treated as a sequence."
  {:added "1.0"
   :static true}
  ([args] (seq args))
  ([a args] (cons a args))
  ([a b args] (cons a (cons b args)))
  ([a b c args] (cons a (cons b (cons c args))))
  ([a b c d & more]
     (cons a (cons b (cons c (cons d (spread more)))))))

(defn unroll-list*-spec*
  ([] (unroll-list*-spec* {}))
  ([{:keys [size] :or {size 4}}]
   {:argvs (let [rest-arity (inc size)]
             (assert (pos? rest-arity))
             (-> (mapv (fn [i]
                         (-> (into [] (take (dec i))
                                   (single-char-syms-from \a))
                             (conj 'args)))
                       (range 1 rest-arity))
                 (conj (-> (into [] (take (dec rest-arity))
                                 (single-char-syms-from \a))
                           (conj '& 'more)))))
    :unroll-arity (fn [{:keys [fixed-args rest-arg]}]
                    (if (and (= 1 (count fixed-args)) (not rest-arg))
                      `(seq ~(first fixed-args))
                      (reduce (fn [acc x] `(cons ~x ~acc))
                              (if rest-arg `(#'clojure.core/spread ~rest-arg) (peek fixed-args))
                              (some-> fixed-args not-empty (cond-> (not rest-arg) pop) rseq))))}))

(def unroll-list*-spec (unroll-list*-spec*))

(deftest unroll-list*-spec-test
  (is (= (prettify-unroll (unroll-arities (unroll-list*-spec* {:size 0})))
         '([& more] ((var cc/spread) more))))
  (is (= (prettify-unroll (unroll-arities (unroll-list*-spec* {:size 1})))
         '(([args] (cc/seq args)) 
           ([a & more] (cc/cons a ((var cc/spread) more))))))
  (is (= (prettify-unroll (unroll-arities (unroll-list*-spec* {:size 2})))
         '(([args] (cc/seq args)) 
           ([a args] (cc/cons a args)) 
           ([a b & more] (cc/cons a (cc/cons b ((var cc/spread) more)))))))
  (is (= (prettify-unroll (unroll-arities unroll-list*-spec))
         '(([args] (cc/seq args))
           ([a args] (cc/cons a args))
           ([a b args] (cc/cons a (cc/cons b args)))
           ([a b c args] (cc/cons a (cc/cons b (cc/cons c args))))
           ([a b c d & more] (cc/cons a (cc/cons b (cc/cons c (cc/cons d ((var cc/spread) more))))))))))

(defunroll unroll-list*
  "Creates a new seq containing the items prepended to the rest, the
  last of which will be treated as a sequence."
  {:added "1.0"
   :static true}
  unroll-list*-spec)

(deftest unroll-list*-test
  (is (= (-> #'unroll-list* meta :arglists)
         (-> #'clojure.core/list* meta :arglists)
         '([args] [a args] [a b args] [a b c args] [a b c d & more])))
  (dotimes [i 10]
    (is (= (apply clojure.core/list* (concat (range i) [(range i)]))
           (apply unroll-list* (concat (range i) [(range i)]))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/apply
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
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

(defn unroll-apply-spec*
  "Generate a spec for an unrolling of clojure.core/apply.
  
   :fixed-arities  Number of fixed arities to generate. Default: 4
   :spread         Form to use for `spread`. Default: `#'clojure.core/spread
   :rest-arg-name  Name of rest argument. Default: 'args
   :f-arg-name     Name of first argument. Default: 'f
   :fixed-arg-names     Infinite sequence of parameter names for fixed arities.
                        Used between first and last parameter. Default: (single-char-syms-from \\x)
   :rest-arity-fixed-arg-names     Infinite sequence of parameter names for fixed params in rest arity.
                        Used after first fixed parameter. Default: (single-char-syms-from \\a)
   :direct-invoke-if-possible    If rest-arg is empty at runtime, directly call the function."
  ([] (unroll-apply-spec* {}))
  ([{:keys [fixed-arities spread rest-arg-name f-arg-name fixed-arg-names rest-arity-fixed-arg-names
            direct-invoke-if-possible]
     :or {fixed-arities 4
          f-arg-name 'f
          rest-arg-name 'args
          fixed-arg-names (single-char-syms-from \x)
          rest-arity-fixed-arg-names (single-char-syms-from \a)
          spread `#'clojure.core/spread}}]
   {:argvs (let [rest-arity (+ 2 fixed-arities)
                 f (with-meta f-arg-name {:tag 'clojure.lang.IFn})
                 args rest-arg-name]
             (-> (mapv (fn [i]
                         (-> [f]
                             (into (take i) fixed-arg-names)
                             (conj args)))
                       (range (- rest-arity 2)))
                 (conj (-> [f]
                           (into (take (- rest-arity 2)) rest-arity-fixed-arg-names)
                           (conj '& args)))))
    :unroll-arity (fn [{[f & fixed-args] :fixed-args :keys [rest-arg]}]
                    (assert f)
                    (let [[fixed-args last-fixed] (if rest-arg
                                                    [fixed-args nil]
                                                    [(butlast fixed-args) (last fixed-args)])
                          slow-case `(. ~f (~'applyTo
                                             ~(if rest-arg
                                                (reduce (fn [acc x] `(cons ~x ~acc))
                                                        `(~spread ~rest-arg)
                                                        (reverse fixed-args))
                                                (if (empty? fixed-args)
                                                  (cond->> last-fixed
                                                    (not direct-invoke-if-possible) (list `seq))
                                                  `(list* ~@fixed-args ~last-fixed)))))]
                      (if (and direct-invoke-if-possible (not rest-arg))
                        `(if-some [~last-fixed (seq ~last-fixed)]
                           ~slow-case
                           (~f ~@fixed-args))
                        slow-case)))}))

(def unroll-apply-spec (unroll-apply-spec*))

(deftest unroll-apply-spec-test
  (is (= (prettify-unroll (unroll-arities (unroll-apply-spec* {:fixed-arities 0})))
         '([f & args] (. f (applyTo ((var cc/spread) args))))))
  (is (= (prettify-unroll (unroll-arities (unroll-apply-spec* {:fixed-arities 1})))
         '(([f args] (. f (applyTo (cc/seq args))))
           ([f a & args] (. f (applyTo (cc/cons a ((var cc/spread) args))))))))
  (is (= (prettify-unroll (unroll-arities (unroll-apply-spec* {:fixed-arities 2})))
         '(([f args] (. f (applyTo (cc/seq args))))
           ([f x args] (. f (applyTo (cc/list* x args))))
           ([f a b & args] (. f (applyTo (cc/cons a (cc/cons b ((var cc/spread) args)))))))))
  (is (= (prettify-unroll (unroll-arities (unroll-apply-spec* {:fixed-arities 3
                                                               :direct-invoke-if-possible true})))
         '(([f args] (cc/if-some [args (cc/seq args)] (. f (applyTo args)) (f)))
           ([f x args] (cc/if-some [args (cc/seq args)] (. f (applyTo (cc/list* x args))) (f x)))
           ([f x y args] (cc/if-some [args (cc/seq args)] (. f (applyTo (cc/list* x y args))) (f x y)))
           ([f a b c & args] (. f (applyTo (cc/cons a (cc/cons b (cc/cons c ((var cc/spread) args))))))))))
  (is (= (prettify-unroll (unroll-arities unroll-apply-spec))
         '(([f args] (. f (applyTo (cc/seq args))))
           ([f x args] (. f (applyTo (cc/list* x args))))
           ([f x y args] (. f (applyTo (cc/list* x y args))))
           ([f x y z args] (. f (applyTo (cc/list* x y z args))))
           ([f a b c d & args] (. f (applyTo (cc/cons a (cc/cons b (cc/cons c (cc/cons d ((var cc/spread) args))))))))))))

(defunroll unroll-apply
  "Applies fn f to the argument list formed by prepending intervening arguments to args."
  {:added "1.0"
   :static true}
  unroll-apply-spec)

(deftest unroll-apply-test
  (is (= (-> #'unroll-apply meta :arglists)
         (-> #'clojure.core/apply meta :arglists)
         '([f args] [f x args] [f x y args] [f x y z args] [f a b c d & args])))
  (dotimes [i 10]
    (is (= (clojure.core/apply clojure.core/list* (concat (range i) [(range i)]))
           (unroll-apply clojure.core/list* (concat (range i) [(range i)]))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/comp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
(defn comp
  "Takes a set of functions and returns a fn that is the composition
  of those fns.  The returned fn takes a variable number of args,
  applies the rightmost of fns to the args, the next
  fn (right-to-left) to the result, etc."
  {:added "1.0"
   :static true}
  ([] identity)
  ([f] f)
  ([f g] 
     (fn 
       ([] (f (g)))
       ([x] (f (g x)))
       ([x y] (f (g x y)))
       ([x y z] (f (g x y z)))
       ([x y z & args] (f (apply g x y z args)))))
  ([f g & fs]
     (reduce1 comp (list* f g fs))))

(defn unroll-comp-spec*
  "If :this is associated to result, will be used for recursive calls to the 2-arity.
  
   :outer-size   Number of fixed arities for outer function. Default: 3
   :inner-size   Number of fixed arities for inner function. Default: 4
   :inner-argvs  A function taking fixed-args and rest-arg of outer arity
                 and returning :argvs for inner. Overrides :inner-size. Default: nil.
   :reduce-fn    Form to use for `reduce`. Default: 'clojure.core/reduce"
  ([] (unroll-comp-spec* {}))
  ([{:keys [outer-size inner-size reduce-fn inner-argvs]
     :or {outer-size 3 inner-size 4 reduce-fn `reduce}}]
   (let [inner-argvs (or inner-argvs
                         (let [argvs (uniformly-flowing-argvs
                                       {:arities (range inner-size)
                                        :fixed-names (single-char-syms-from \x)
                                        :rest-name 'args})]
                           (fn [_ _] argvs)))]
     {:argvs (uniformly-flowing-argvs
               {:arities (range outer-size)
                :fixed-names (single-char-syms-from \f)
                :rest-name 'fs})
      :unroll-arity (fn [{:keys [this] fixed-fs :fixed-args rest-fs :rest-arg}]
                      (if rest-fs
                        `(~reduce-fn ~(or (when (<= 2 (count fixed-fs)) this)
                                          (let [[f g args] (map gensym-pretty '[f g args])]
                                            `(fn ~@(flatten-arities
                                                     (cond->> [`([~f ~g] (fn [& ~args] (~f (apply ~g ~args))))]
                                                       (empty? fixed-fs) (cons `([] identity)))))))
                                     ~@(some-> (first fixed-fs) list)
                                     ~(maybe-list* (next fixed-fs) rest-fs))
                        (case (count fixed-fs)
                          0 `identity
                          1 (first fixed-fs)
                          `(fn ~@(unroll-arities
                                   {:argvs (inner-argvs fixed-fs rest-fs)
                                    :unroll-arity (fn [{:keys [fixed-args rest-arg]}]
                                                    (reduce (fn [acc outer-f]
                                                              (list outer-f acc))
                                                            (maybe-apply (peek fixed-fs) fixed-args rest-arg)
                                                            (rseq (pop fixed-fs))))})))))})))

(def unroll-comp-spec (unroll-comp-spec*))

(deftest unroll-comp-spec-test
  (is (= (prettify-unroll (unroll-arities (assoc (unroll-comp-spec* {:outer-size 0 :inner-size 0}) :this 'this/comp)))
         (prettify-unroll (unroll-arities (unroll-comp-spec* {:outer-size 0 :inner-size 0})))
         '([& fs] (cc/reduce (cc/fn
                               ([] cc/identity)
                               ([f g] (cc/fn [& args] (f (cc/apply g args)))))
                             fs))))
  (is (= (prettify-unroll (unroll-arities (assoc (unroll-comp-spec* {:outer-size 1 :inner-size 0}) :this 'this/comp)))
         '(([] cc/identity)
           ([& fs] (cc/reduce (cc/fn
                                ([] cc/identity)
                                ([f g] (cc/fn [& args] (f (cc/apply g args)))))
                              fs)))))
  (is (= (prettify-unroll (unroll-arities (assoc (unroll-comp-spec* {:outer-size 2 :inner-size 0}) :this 'this/comp)))
         '(([] cc/identity)
           ([f] f)
           ([f & fs] (cc/reduce (cc/fn [f g]
                                  (cc/fn [& args] (f (cc/apply g args))))
                                f
                                fs)))))
  (is (= (prettify-unroll (unroll-arities (assoc (unroll-comp-spec* {:outer-size 3 :inner-size 0}) :this 'this/comp)))
         '(([] cc/identity)
           ([f] f)
           ([f g] (cc/fn [& args] (f (cc/apply g args))))
           ([f g & fs] (cc/reduce this/comp f (cc/list* g fs))))))
  (is (= (prettify-unroll (unroll-arities (assoc (unroll-comp-spec* {:outer-size 3 :inner-size 1}) :this 'this/comp)))
         '(([] cc/identity)
           ([f] f)
           ([f g] (cc/fn
                    ([] (f (g)))
                    ([& args] (f (cc/apply g args)))))
           ([f g & fs] (cc/reduce this/comp f (cc/list* g fs))))))
  (is (= (prettify-unroll (unroll-arities (assoc (unroll-comp-spec* {:outer-size 3 :inner-size 2}) :this 'this/comp)))
         '(([] cc/identity)
           ([f] f)
           ([f g] (cc/fn
                    ([] (f (g)))
                    ([x] (f (g x)))
                    ([x & args] (f (cc/apply g x args)))))
           ([f g & fs] (cc/reduce this/comp f (cc/list* g fs))))))
  (is (= (prettify-unroll (unroll-arities (assoc (unroll-comp-spec* {:outer-size 4 :inner-size 0}) :this 'this/comp)))
         '(([] cc/identity)
           ([f] f)
           ([f g] (cc/fn [& args] (f (cc/apply g args))))
           ([f g h] (cc/fn [& args] (f (g (cc/apply h args)))))
           ([f g h & fs] (cc/reduce this/comp f (cc/list* g h fs))))))
  (is (= (prettify-unroll (unroll-arities (assoc (unroll-comp-spec* {:outer-size 5 :inner-size 3}) :this 'this/comp)))
         '(([] cc/identity)
           ([f] f)
           ([f g] (cc/fn ([] (f (g))) ([x] (f (g x))) ([x y] (f (g x y))) ([x y & args] (f (cc/apply g x y args)))))
           ([f g h] (cc/fn ([] (f (g (h)))) ([x] (f (g (h x)))) ([x y] (f (g (h x y)))) ([x y & args] (f (g (cc/apply h x y args))))))
           ([f g h i] (cc/fn ([] (f (g (h (i))))) ([x] (f (g (h (i x))))) ([x y] (f (g (h (i x y))))) ([x y & args] (f (g (h (cc/apply i x y args)))))))
           ([f g h i & fs] (cc/reduce this/comp f (cc/list* g h i fs))))))
  (is (= (prettify-unroll (unroll-arities (assoc unroll-comp-spec :this 'this/comp)))
         '(([] cc/identity)
           ([f] f)
           ([f g] (cc/fn 
                    ([] (f (g)))
                    ([x] (f (g x)))
                    ([x y] (f (g x y)))
                    ([x y z] (f (g x y z)))
                    ([x y z & args] (f (cc/apply g x y z args)))))
           ([f g & fs] (cc/reduce this/comp f (cc/list* g fs)))))))

(defunroll unroll-comp
  "Takes a set of functions and returns a fn that is the composition
  of those fns.  The returned fn takes a variable number of args,
  applies the rightmost of fns to the args, the next
  fn (right-to-left) to the result, etc."
  {:added "1.0"
   :static true}
  unroll-comp-spec)

(deftest unroll-comp-test
  (is (= (-> #'unroll-comp meta :arglists)
         (-> #'clojure.core/comp meta :arglists)
         '([] [f] [f g] [f g & fs])))
  (is (= (clojure.core/comp) (unroll-comp)))
  (is (= + (clojure.core/comp +) (unroll-comp +)))
  (doseq [i (range 1 10)]
    (is (= (apply (apply clojure.core/comp (concat (repeat i inc) [+]))
                  (range i))
           (apply (apply unroll-comp (concat (repeat i inc) [+]))
                  (range i))))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/juxt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
(defn juxt 
  "Takes a set of functions and returns a fn that is the juxtaposition
  of those fns.  The returned fn takes a variable number of args, and
  returns a vector containing the result of applying each fn to the
  args (left-to-right).
  ((juxt a b c) x) => [(a x) (b x) (c x)]"
  {:added "1.1"
   :static true}
  ([f] 
     (fn
       ([] [(f)])
       ([x] [(f x)])
       ([x y] [(f x y)])
       ([x y z] [(f x y z)])
       ([x y z & args] [(apply f x y z args)])))
  ([f g] 
     (fn
       ([] [(f) (g)])
       ([x] [(f x) (g x)])
       ([x y] [(f x y) (g x y)])
       ([x y z] [(f x y z) (g x y z)])
       ([x y z & args] [(apply f x y z args) (apply g x y z args)])))
  ([f g h] 
     (fn
       ([] [(f) (g) (h)])
       ([x] [(f x) (g x) (h x)])
       ([x y] [(f x y) (g x y) (h x y)])
       ([x y z] [(f x y z) (g x y z) (h x y z)])
       ([x y z & args] [(apply f x y z args) (apply g x y z args) (apply h x y z args)])))
  ([f g h & fs]
     (let [fs (list* f g h fs)]
       (fn
         ([] (reduce1 #(conj %1 (%2)) [] fs))
         ([x] (reduce1 #(conj %1 (%2 x)) [] fs))
         ([x y] (reduce1 #(conj %1 (%2 x y)) [] fs))
         ([x y z] (reduce1 #(conj %1 (%2 x y z)) [] fs))
         ([x y z & args] (reduce1 #(conj %1 (apply %2 x y z args)) [] fs))))))

(defn unroll-juxt-spec*
  ([] (unroll-juxt-spec* {}))
  ([{:keys [outer-size inner-size]}] ;;FIXME use params in body and set defaults
   {:argvs (uniformly-flowing-argvs
             {:arities (range 3)
              :leading-names ['f]
              :fixed-names (single-char-syms-from \g)
              :rest-name 'fs})
    :unroll-arity (fn [{fixed-fs :fixed-args rest-fs :rest-arg}]
                    (let [fs (gensym-pretty 'fs)
                          body `(fn ~@(unroll-arities
                                        {:argvs (uniformly-flowing-argvs
                                                  {:arities (range 4)
                                                   :fixed-names (single-char-syms-from \x)
                                                   :rest-name 'args})
                                         :unroll-arity (fn [{:keys [fixed-args rest-arg]}]
                                                         (if rest-fs
                                                           (let [v (gensym-pretty 'acc)
                                                                 f (gensym-pretty 'f)]
                                                             `(reduce (fn [~v ~f] (conj ~v ~(maybe-apply f fixed-args rest-arg))) [] ~fs))
                                                           (mapv #(maybe-apply % fixed-args rest-arg) fixed-fs)))}))]
                      (if rest-fs
                        `(let [~fs ~(maybe-list* fixed-fs rest-fs)] ~body)
                        body)))}))

(def unroll-juxt-spec (unroll-juxt-spec*))

(deftest unroll-juxt-spec-test
  (is (= (prettify-unroll (unroll-arities unroll-juxt-spec))
         '(([f]
            (cc/fn
              ([] [(f)])
              ([x] [(f x)])
              ([x y] [(f x y)])
              ([x y z] [(f x y z)])
              ([x y z & args] [(cc/apply f x y z args)])))
           ([f g]
            (cc/fn
              ([] [(f) (g)])
              ([x] [(f x) (g x)])
              ([x y] [(f x y) (g x y)])
              ([x y z] [(f x y z) (g x y z)])
              ([x y z & args] [(cc/apply f x y z args) (cc/apply g x y z args)])))
           ([f g h]
            (cc/fn
              ([] [(f) (g) (h)])
              ([x] [(f x) (g x) (h x)])
              ([x y] [(f x y) (g x y) (h x y)])
              ([x y z] [(f x y z) (g x y z) (h x y z)])
              ([x y z & args] [(cc/apply f x y z args) (cc/apply g x y z args) (cc/apply h x y z args)])))
           ([f g h & fs]
            (cc/let [fs (cc/list* f g h fs)]
              (cc/fn
                ([] (cc/reduce (cc/fn [acc f] (cc/conj acc (f))) [] fs))
                ([x] (cc/reduce (cc/fn [acc f] (cc/conj acc (f x))) [] fs))
                ([x y] (cc/reduce (cc/fn [acc f] (cc/conj acc (f x y))) [] fs))
                ([x y z] (cc/reduce (cc/fn [acc f] (cc/conj acc (f x y z))) [] fs))
                ([x y z & args] (cc/reduce (cc/fn [acc f] (cc/conj acc (cc/apply f x y z args))) [] fs)))))))))

(defunroll unroll-juxt 
  "Takes a set of functions and returns a fn that is the juxtaposition
  of those fns.  The returned fn takes a variable number of args, and
  returns a vector containing the result of applying each fn to the
  args (left-to-right).
  ((juxt a b c) x) => [(a x) (b x) (c x)]"
  {:added "1.1"
   :static true}
  unroll-juxt-spec)

(deftest unroll-juxt-test
  (is (= (-> #'unroll-juxt meta :arglists)
         (-> #'clojure.core/juxt meta :arglists)
         '([f] [f g] [f g h] [f g h & fs])))
  (doseq [i (range 1 10)]
    (is (= (apply (apply clojure.core/juxt (repeat i vector))
                  (range i))
           (apply (apply unroll-juxt (repeat i vector))
                  (range i))))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/partial
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#_
(defn partial
  "Takes a function f and fewer than the normal arguments to f, and
  returns a fn that takes a variable number of additional args. When
  called, the returned function calls f with args + additional args."
  {:added "1.0"
   :static true}
  ([f] f)
  ([f arg1]
   (fn
     ([] (f arg1))
     ([x] (f arg1 x))
     ([x y] (f arg1 x y))
     ([x y z] (f arg1 x y z))
     ([x y z & args] (apply f arg1 x y z args))))
  ([f arg1 arg2]
   (fn
     ([] (f arg1 arg2))
     ([x] (f arg1 arg2 x))
     ([x y] (f arg1 arg2 x y))
     ([x y z] (f arg1 arg2 x y z))
     ([x y z & args] (apply f arg1 arg2 x y z args))))
  ([f arg1 arg2 arg3]
   (fn
     ([] (f arg1 arg2 arg3))
     ([x] (f arg1 arg2 arg3 x))
     ([x y] (f arg1 arg2 arg3 x y))
     ([x y z] (f arg1 arg2 arg3 x y z))
     ([x y z & args] (apply f arg1 arg2 arg3 x y z args))))
  ([f arg1 arg2 arg3 & more]
   (fn [& args] (apply f arg1 arg2 arg3 (concat more args)))))

(def unroll-partial-spec
  {:argvs (uniformly-flowing-argvs
            {:arities (range 4)
             :leading-names ['f]
             :fixed-names (map #(symbol (str "arg" %)) (next (range)))
             :rest-name 'more})
   :unroll-arity (fn [{[f & fixed-args] :fixed-args :keys [rest-arg]}]
                   (if (and (not fixed-args) (not rest-arg))
                     f ;; don't eta expand
                     `(fn ~@(unroll-arities
                              {:argvs (uniformly-flowing-argvs
                                        {:arities (range (if rest-arg 0 4))
                                         :fixed-names (single-char-syms-from \x)
                                         :rest-name 'args})
                               :unroll-arity (fn [{fixed-additional-args :fixed-args rest-additional-args :rest-arg}]
                                               (maybe-apply f
                                                            (concat fixed-args fixed-additional-args)
                                                            (maybe-concat rest-arg rest-additional-args)))}))))})

(deftest unroll-partial-spec-test
  (is (= (prettify-unroll (unroll-arities unroll-partial-spec))
         '(([f] f)
           ([f arg1]
            (cc/fn
              ([] (f arg1))
              ([x] (f arg1 x))
              ([x y] (f arg1 x y))
              ([x y z] (f arg1 x y z))
              ([x y z & args] (cc/apply f arg1 x y z args))))
           ([f arg1 arg2]
            (cc/fn
              ([] (f arg1 arg2))
              ([x] (f arg1 arg2 x))
              ([x y] (f arg1 arg2 x y))
              ([x y z] (f arg1 arg2 x y z))
              ([x y z & args] (cc/apply f arg1 arg2 x y z args))))
           ([f arg1 arg2 arg3]
            (cc/fn
              ([] (f arg1 arg2 arg3))
              ([x] (f arg1 arg2 arg3 x))
              ([x y] (f arg1 arg2 arg3 x y))
              ([x y z] (f arg1 arg2 arg3 x y z))
              ([x y z & args] (cc/apply f arg1 arg2 arg3 x y z args))))
           ([f arg1 arg2 arg3 & more]
            (cc/fn [& args] (cc/apply f arg1 arg2 arg3 (cc/concat more args))))))))

(defunroll unroll-partial
  "Takes a function f and fewer than the normal arguments to f, and
  returns a fn that takes a variable number of additional args. When
  called, the returned function calls f with args + additional args."
  {:added "1.0"
   :static true}
  unroll-partial-spec)

(deftest unroll-partial-test
  (is (= (-> #'unroll-partial meta :arglists)
         (-> #'clojure.core/partial meta :arglists)
         '([f] [f arg1] [f arg1 arg2] [f arg1 arg2 arg3] [f arg1 arg2 arg3 & more])))
  (is (= (partial identity) (unroll-partial identity)))
  (doseq [i (range 1 10)]
    (is (= (apply (apply clojure.core/partial vector (range i))
                  (range i))
           (apply (apply unroll-partial vector (range i))
                  (range i))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; everyp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#_ ;; naive expansion of (fixed) everyp
(defn everyp
  ([]
   (fn
     ([] (boolean (and)))
     ([x] (boolean (and)))
     ([x y] (boolean (and)))
     ([x y z] (boolean (and)))
     ([x y z & args] (boolean (and)))))
  ([p1]
   (fn
     ([] (boolean (and)))
     ([x] (boolean (and (p1 x))))
     ([x y] (boolean (and (p1 x) (p1 y))))
     ([x y z] (boolean (and (p1 x) (p1 y) (p1 z))))
     ([x y z & args] (boolean (and (p1 x) (p1 y) (p1 z) (every? p1 args))))))
  ([p1 p2]
   (fn
     ([] (boolean (and)))
     ([x] (boolean (and (p1 x)
                        (p2 x))))
     ([x y] (boolean (and (p1 x) (p1 y)
                          (p2 x) (p2 y))))
     ([x y z] (boolean (and (p1 x) (p1 y) (p1 z)
                            (p2 x) (p2 y) (p2 z))))
     ([x y z & args] (boolean (and (p1 x) (p1 y) (p1 z) (every? p1 args)
                                   (p2 x) (p2 y) (p2 z) (every? p2 args))))))
  ([p1 p2 p3]
   (fn
     ([] (boolean (and)))
     ([x] (boolean (and (p1 x)
                        (p2 x)
                        (p3 x))))
     ([x y] (boolean (and (p1 x) (p1 y)
                          (p2 x) (p2 y)
                          (p3 x) (p3 y))))
     ([x y z] (boolean (and (p1 x) (p1 y) (p1 z)
                            (p2 x) (p2 y) (p2 z)
                            (p3 x) (p3 y) (p3 z))))
     ([x y z & args] (boolean (and (p1 x) (p1 y) (p1 z) (every? p1 args)
                                   (p2 x) (p2 y) (p2 z) (every? p2 args)
                                   (p3 x) (p3 y) (p3 z) (every? p3 args))))))
  ([p1 p2 p3 & ps]
   (fn
     ([] (boolean (and (every? (fn [p] (and)) ps))))
     ([x] (boolean (and (p1 x) (p2 x) (p3 x)
                        (every? (fn [p] (and (p x))) ps))))
     ([x y] (boolean (and (p1 x) (p1 y) (p1 z)
                          (p2 x) (p2 y) (p2 z)
                          (every? (fn [p] (and (p x) (p y))) ps))))
     ([x y z] (boolean (and (p1 x) (p1 y) (p1 z)
                            (p2 x) (p2 y) (p2 z)
                            (p3 x) (p3 y) (p3 z)
                            (every? (fn [p] (and (p x) (p y) (p z))) ps))))
     ([x y z & args] (boolean (and (p1 x) (p1 y) (p1 z) (every? p1 args)
                                   (p2 x) (p2 y) (p2 z) (every? p2 args)
                                   (p3 x) (p3 y) (p3 z) (every? p3 args)
                                   (every? (fn [p] (and (p x) (p y) (p z) (every? p args))) ps)))))))

(defn unroll-everyp-or-somef-spec*
  ":use-local-helper   A predicate taking {:outer-argv outer-argv :inner-argv inner-argv}. Return
                    a true value to bind a local function called `tp` to better manage method size.
                    Default: nil (inline everything)
   :outer-size      Number of fixed arities for outer fn that takes predicates.
   :outer-argv->inner-size      Function from outer argv to number of fixed arities for inner fn that takes args."
  ([{:keys [mode outer-size outer-argv->inner-size use-local-helper]
     :or {outer-size 4
          outer-argv->inner-size (constantly 4)}}]
   (assert (nat-int? outer-size))
   (assert (#{:everyp :somef} mode))
   {:argvs (uniformly-flowing-argvs
             {:arities (range outer-size)
              :fixed-names (map (fn [i]
                                  (symbol (str (case mode
                                                 :everyp 'p
                                                 :somef 'f)
                                               i)))
                                (next (range)))
              :rest-name (case mode
                           :everyp 'ps
                           :somef 'fs)})
    :unroll-arity (fn [{fixed-preds :fixed-args rest-pred :rest-arg outer-argv :argv}]
                    `(fn ~@(unroll-arities
                             {:argvs (uniformly-flowing-argvs
                                       {:arities (range (outer-argv->inner-size outer-argv))
                                        :fixed-names (single-char-syms-from \x)
                                        :rest-name 'args})
                              :unroll-arity (fn [{:keys [fixed-args rest-arg] inner-argv :argv}]
                                              (let [tp (when (and use-local-helper
                                                                  (< 1 (cond-> (count fixed-preds) rest-pred inc))
                                                                  (< 1 (cond-> (count fixed-args) rest-arg inc))
                                                                  (use-local-helper {:outer-argv outer-argv :inner-argv inner-argv}))
                                                         (gensym-pretty (case mode
                                                                          :everyp 'tp
                                                                          :somef 'tf)))
                                                    maybe-combine-rest (case mode
                                                                         :everyp maybe-every?
                                                                         :somef maybe-some)
                                                    maybe-combine-fixed-outer (case mode
                                                                                :everyp (comp maybe-boolean maybe-and)
                                                                                :somef (if (and tp rest-arg) maybe-or maybe-or-nil))
                                                    maybe-combine-fixed-inner (case mode
                                                                                :everyp maybe-and
                                                                                :somef maybe-or)
                                                    tp-gen (fn [p]
                                                             (cond-> (mapv #(list p %) fixed-args)
                                                               rest-arg (conj (maybe-combine-rest p rest-arg))))
                                                    tp-call (fn [p] (if tp [`(~tp ~p)] (tp-gen p)))
                                                    p (gensym-pretty (case mode
                                                                       :everyp 'p
                                                                       :somef 'f))
                                                    body (maybe-combine-fixed-outer
                                                           (cond-> (into [] (mapcat tp-call) fixed-preds)
                                                             rest-pred (conj (maybe-combine-rest (or tp `(fn [~p] ~(maybe-combine-fixed-inner (tp-call p)))) rest-pred))))]
                                                (if tp
                                                  `(let [~tp (fn [~p] ~(maybe-combine-fixed-inner (tp-gen p)))] ~body)
                                                  body)))})))}))

(defn unroll-everyp-spec*
  ":use-local-helper   A predicate taking {:outer-argv outer-argv :inner-argv inner-argv}. Return
                    a true value to bind a local function called `tp` to better manage method size.
                    Default: nil (inline everything)
   :outer-size      Number of fixed arities for outer fn that takes predicates.
   :outer-argv->inner-size      Function from outer argv to number of fixed arities for inner fn that takes args."
  ([] (unroll-everyp-spec* {}))
  ([opt] (unroll-everyp-or-somef-spec* (assoc opt :mode :everyp))))

(def unroll-everyp-spec (unroll-everyp-spec*))

(deftest unroll-everyp-spec-test
  (is (= (prettify-unroll (unroll-arities (unroll-everyp-spec*
                                            {:outer-size 0
                                             :outer-argv->inner-size (constantly 0)})))
         '([& ps] (cc/fn [& args] (cc/every? (cc/fn [p] (cc/every? p args)) ps)))))
  (is (= (prettify-unroll (unroll-arities (unroll-everyp-spec*
                                            {:outer-size 1
                                             :outer-argv->inner-size (constantly 0)})))

         '(([] (cc/fn [& args] true))
           ([& ps] (cc/fn [& args] (cc/every? (cc/fn [p] (cc/every? p args)) ps))))))
  (is (= (prettify-unroll (unroll-arities (unroll-everyp-spec*
                                            {:outer-size 1
                                             :outer-argv->inner-size (constantly 1)})))


         '(([] (cc/fn
                 ([] true)
                 ([& args] true)))
           ([& ps] (cc/fn
                     ([] true)
                     ([& args] (cc/every? (cc/fn [p] (cc/every? p args)) ps)))))))
  (is (= (prettify-unroll (unroll-arities (unroll-everyp-spec*
                                            {:outer-size 3
                                             :outer-argv->inner-size (constantly 4)
                                             ;; let-bind tp only on & args
                                             :use-local-helper (comp argv->rest-arg :inner-argv)})))
         '(([] (cc/fn ([] true) ([x] true) ([x y] true) ([x y z] true) ([x y z & args] true)))
           ([p1] (cc/fn
                   ([] true)
                   ([x] (cc/boolean (p1 x)))
                   ([x y] (cc/boolean (cc/and (p1 x) (p1 y))))
                   ([x y z] (cc/boolean (cc/and (p1 x) (p1 y) (p1 z))))
                   ([x y z & args] (cc/boolean (cc/and (p1 x) (p1 y) (p1 z) (cc/every? p1 args))))))
           ([p1 p2] (cc/fn
                      ([] true)
                      ([x] (cc/boolean (cc/and (p1 x) (p2 x))))
                      ([x y] (cc/boolean (cc/and (p1 x) (p1 y) (p2 x) (p2 y))))
                      ([x y z] (cc/boolean (cc/and (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z))))
                      ([x y z & args] (cc/let [tp (cc/fn [p] (cc/and (p x) (p y) (p z) (cc/every? p args)))]
                                        (cc/boolean (cc/and (tp p1) (tp p2)))))))
           ([p1 p2 & ps] (cc/fn
                           ([] true)
                           ([x] (cc/boolean (cc/and (p1 x) (p2 x) (cc/every? (cc/fn [p] (p x)) ps))))
                           ([x y] (cc/boolean (cc/and (p1 x) (p1 y) (p2 x) (p2 y) (cc/every? (cc/fn [p] (cc/and (p x) (p y))) ps))))
                           ([x y z] (cc/boolean (cc/and (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z) (cc/every? (cc/fn [p] (cc/and (p x) (p y) (p z))) ps))))
                           ([x y z & args] (cc/let [tp (cc/fn [p] (cc/and (p x) (p y) (p z) (cc/every? p args)))]
                                             (cc/boolean (cc/and (tp p1) (tp p2) (cc/every? tp ps))))))))))
  ;; potentially useful implementation. maybe the non-rest arities should fully unroll.
  (is (= (prettify-unroll (unroll-arities (unroll-everyp-spec*
                                            {:use-local-helper (comp argv->rest-arg :inner-argv)}))
                          {:unqualify-core true})

         '(([] (fn
                 ([] true)
                 ([x] true)
                 ([x y] true)
                 ([x y z] true)
                 ([x y z & args] true)))
           ([p1] (fn
                   ([] true)
                   ([x] (boolean (p1 x)))
                   ([x y] (boolean (and (p1 x) (p1 y))))
                   ([x y z] (boolean (and (p1 x) (p1 y) (p1 z))))
                   ([x y z & args] (boolean (and (p1 x) (p1 y) (p1 z) (every? p1 args))))))
           ([p1 p2] (fn
                      ([] true)
                      ([x] (boolean (and (p1 x) (p2 x))))
                      ([x y] (boolean (and (p1 x) (p1 y) (p2 x) (p2 y))))
                      ([x y z] (boolean (and (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z))))
                      ([x y z & args] (let [tp (fn [p] (and (p x) (p y) (p z) (every? p args)))]
                                        (boolean (and (tp p1) (tp p2)))))))
           ([p1 p2 p3] (fn
                         ([] true)
                         ([x] (boolean (and (p1 x) (p2 x) (p3 x))))
                         ([x y] (boolean (and (p1 x) (p1 y) (p2 x) (p2 y) (p3 x) (p3 y))))
                         ([x y z] (boolean (and (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z) (p3 x) (p3 y) (p3 z))))
                         ([x y z & args] (let [tp (fn [p] (and (p x) (p y) (p z) (every? p args)))]
                                           (boolean (and (tp p1) (tp p2) (tp p3)))))))
           ([p1 p2 p3 & ps] (fn
                              ([] true)
                              ([x] (boolean (and (p1 x) (p2 x) (p3 x) (every? (fn [p] (p x)) ps))))
                              ([x y] (boolean (and (p1 x) (p1 y) (p2 x) (p2 y) (p3 x) (p3 y) (every? (fn [p] (and (p x) (p y))) ps))))
                              ([x y z] (boolean (and (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z) (p3 x) (p3 y) (p3 z) (every? (fn [p] (and (p x) (p y) (p z))) ps))))
                              ([x y z & args] (let [tp (fn [p] (and (p x) (p y) (p z) (every? p args)))]
                                                (boolean (and (tp p1) (tp p2) (tp p3) (every? tp ps))))))))))
  (is (= (prettify-unroll (unroll-arities unroll-everyp-spec))
         '(([] (cc/fn
                 ([] true)
                 ([x] true)
                 ([x y] true)
                 ([x y z] true)
                 ([x y z & args] true)))
           ([p1] (cc/fn
                   ([] true)
                   ([x] (cc/boolean (p1 x)))
                   ([x y] (cc/boolean (cc/and (p1 x) (p1 y))))
                   ([x y z] (cc/boolean (cc/and (p1 x) (p1 y) (p1 z))))
                   ([x y z & args] (cc/boolean (cc/and (p1 x) (p1 y) (p1 z) (cc/every? p1 args))))))
           ([p1 p2] (cc/fn
                      ([] true)
                      ([x] (cc/boolean (cc/and (p1 x) (p2 x))))
                      ([x y] (cc/boolean (cc/and (p1 x) (p1 y) (p2 x) (p2 y))))
                      ([x y z] (cc/boolean (cc/and (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z))))
                      ([x y z & args] (cc/boolean (cc/and (p1 x) (p1 y) (p1 z) (cc/every? p1 args) (p2 x) (p2 y) (p2 z) (cc/every? p2 args))))))
           ([p1 p2 p3] (cc/fn
                         ([] true)
                         ([x] (cc/boolean (cc/and (p1 x) (p2 x) (p3 x))))
                         ([x y] (cc/boolean (cc/and (p1 x) (p1 y) (p2 x) (p2 y) (p3 x) (p3 y))))
                         ([x y z] (cc/boolean (cc/and (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z) (p3 x) (p3 y) (p3 z))))
                         ([x y z & args] (cc/boolean (cc/and (p1 x) (p1 y) (p1 z) (cc/every? p1 args) (p2 x) (p2 y) (p2 z) (cc/every? p2 args) (p3 x) (p3 y) (p3 z) (cc/every? p3 args))))))
           ([p1 p2 p3 & ps] (cc/fn
                              ([] true)
                              ([x] (cc/boolean (cc/and (p1 x) (p2 x) (p3 x) (cc/every? (cc/fn [p] (p x)) ps))))
                              ([x y] (cc/boolean (cc/and (p1 x) (p1 y) (p2 x) (p2 y) (p3 x) (p3 y) (cc/every? (cc/fn [p] (cc/and (p x) (p y))) ps))))
                              ([x y z] (cc/boolean (cc/and (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z) (p3 x) (p3 y) (p3 z) (cc/every? (cc/fn [p] (cc/and (p x) (p y) (p z))) ps))))
                              ([x y z & args] (cc/boolean (cc/and (p1 x) (p1 y) (p1 z) (cc/every? p1 args)
                                                                  (p2 x) (p2 y) (p2 z) (cc/every? p2 args)
                                                                  (p3 x) (p3 y) (p3 z) (cc/every? p3 args)
                                                                  (cc/every? (cc/fn [p] (cc/and (p x) (p y) (p z) (cc/every? p args))) ps))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Naive somef
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#_
(defn somef
  "Combines functions into a variable-arity disjunction.
  
  Definitionally equivalent to:

    (defn somef [& fs]
      (fn [& args] (some #(some % args) fs)))"
  {:arglists '([& fs])}
  ([]
     (fn
       ([] nil)
       ([x] nil)
       ([x y] nil)
       ([x y z] nil)
       ([x y z & args] nil)))
  ([f1]
     (fn
       ([] nil)
       ([x] (or (f1 x) nil))
       ([x y] (or (f1 x) (f1 y)
                  nil))
       ([x y z] (or (f1 x) (f1 y) (f1 z)
                    nil))
       ([x y z & args] (or (f1 x) (f1 y) (f1 z) (some f1 args)))))
  ([f1 f2]
     (fn
       ([] nil)
       ([x] (or (f1 x) (f2 x)
                nil))
       ([x y] (or (f1 x) (f1 y)
                  (f2 x) (f2 y)
                  nil))
       ([x y z] (or (f1 x) (f1 y) (f1 z)
                    (f2 x) (f2 y) (f2 z)
                    nil))
       ([x y z & args] (or (f1 x) (f1 y) (f1 z) (some f1 args)
                           (f2 x) (f2 y) (f2 z) (some f2 args)))))
  ([f1 f2 f3]
     (fn
       ([] nil)
       ([x] (or (f1 x) (f2 x) (f3 x)
                nil))
       ([x y] (or (f1 x) (f1 y)
                  (f2 x) (f2 y)
                  (f3 x) (f3 y)
                  nil))
       ([x y z] (or (f1 x) (f1 y) (f1 z)
                    (f2 x) (f2 y) (f2 z)
                    (f3 x) (f3 y) (f3 z)
                    nil))
       ([x y z & args] (or (f1 x) (f1 y) (f1 z) (some f1 args)
                           (f2 x) (f2 y) (f2 z) (some f2 args)
                           (f3 x) (f3 y) (f3 z) (some f3 args)))))
  ([f1 f2 f3 & fs]
   (fn
     ([] nil)
     ([x] (or (f1 x) (f2 x) (f3 x)
              (some #(or (% x)) fs)))
     ([x y] (or (f1 x) (f1 y)
                (f2 x) (f2 y)
                (f3 x) (f3 y)
                (some #(or (% x) (% y)) fs)))
     ([x y z] (or (f1 x) (f1 y) (f1 z)
                  (f2 x) (f2 y) (f2 z)
                  (f3 x) (f3 y) (f3 z)
                  (some #(or (% x) (% y) (% z)) fs)))
     ([x y z & args] (or (f1 x) (f1 y) (f1 z) (some f1 args)
                         (f2 x) (f2 y) (f2 z) (some f2 args)
                         (f3 x) (f3 y) (f3 z) (some f3 args)
                         (some #(or (% x) (% y) (% z) (some % args)) fs))))))

;; TODO unit test
(defn unroll-somef-spec*
  ":use-local-helper   A predicate taking {:outer-argv outer-argv :inner-argv inner-argv}. Return
                    a true value to bind a local function called `tf` to better manage method size.
                    Default: nil (inline everything)
   :outer-size      Number of fixed arities for outer fn that takes predicates.
   :outer-argv->inner-size      Function from outer argv to number of fixed arities for inner fn that takes args."
  ([] (unroll-somef-spec* {}))
  ([opt] (unroll-everyp-or-somef-spec* (assoc opt :mode :somef))))

(def unroll-somef-spec (unroll-somef-spec*))

(deftest unroll-somef-spec-test
  (is (= (prettify-unroll (unroll-arities (unroll-somef-spec*
                                            {:outer-size 0
                                             :outer-argv->inner-size (constantly 0)})))
         '([& fs] (cc/fn [& args] (cc/some (cc/fn [f] (cc/some f args)) fs)))))
  (is (= (prettify-unroll (unroll-arities (unroll-somef-spec*
                                            {:outer-size 1
                                             :outer-argv->inner-size (constantly 0)})))

         '(([] (cc/fn [& args] nil))
           ([& fs] (cc/fn [& args] (cc/some (cc/fn [f] (cc/some f args)) fs))))))
  (is (= (prettify-unroll (unroll-arities (unroll-somef-spec*
                                            {:outer-size 1
                                             :outer-argv->inner-size (constantly 1)})))


         '(([] (cc/fn ([] nil) ([& args] nil)))
           ([& fs] (cc/fn ([] nil) ([& args] (cc/some (cc/fn [f] (cc/some f args)) fs)))))))
  (is (= (prettify-unroll (unroll-arities (unroll-somef-spec*
                                            {:outer-size 3
                                             :outer-argv->inner-size (constantly 4)
                                             ;; let-bind tf only on & args
                                             :use-local-helper (comp argv->rest-arg :inner-argv)})))

         '(([] (cc/fn ([] nil) ([x] nil) ([x y] nil) ([x y z] nil) ([x y z & args] nil)))
           ([f1] (cc/fn 
                   ([] nil)
                   ([x] (cc/or (f1 x) nil))
                   ([x y] (cc/or (f1 x) (f1 y) nil))
                   ([x y z] (cc/or (f1 x) (f1 y) (f1 z) nil))
                   ([x y z & args] (cc/or (f1 x) (f1 y) (f1 z) (cc/some f1 args)))))
           ([f1 f2] (cc/fn
                      ([] nil)
                      ([x] (cc/or (f1 x) (f2 x) nil))
                      ([x y] (cc/or (f1 x) (f1 y) (f2 x) (f2 y) nil))
                      ([x y z] (cc/or (f1 x) (f1 y) (f1 z) (f2 x) (f2 y) (f2 z) nil))
                      ([x y z & args] (cc/let [tf (cc/fn [f] (cc/or (f x) (f y) (f z) (cc/some f args)))]
                                        (cc/or (tf f1) (tf f2))))))
           ([f1 f2 & fs] (cc/fn
                           ([] nil)
                           ([x] (cc/or (f1 x) (f2 x) (cc/some (cc/fn [f] (f x)) fs)))
                           ([x y] (cc/or (f1 x) (f1 y) (f2 x) (f2 y) (cc/some (cc/fn [f] (cc/or (f x) (f y))) fs)))
                           ([x y z] (cc/or (f1 x) (f1 y) (f1 z) (f2 x) (f2 y) (f2 z) (cc/some (cc/fn [f] (cc/or (f x) (f y) (f z))) fs)))
                           ([x y z & args] (cc/let [tf (cc/fn [f] (cc/or (f x) (f y) (f z) (cc/some f args)))]
                                             (cc/or (tf f1) (tf f2) (cc/some tf fs)))))))))
  (is (= (prettify-unroll (unroll-arities (unroll-somef-spec*
                                            {:use-local-helper (comp argv->rest-arg :inner-argv)}))
                          {:unqualify-core true})

         '(([] (fn ([] nil) ([x] nil) ([x y] nil) ([x y z] nil) ([x y z & args] nil)))
           ([f1] (fn
                   ([] nil)
                   ([x] (or (f1 x) nil))
                   ([x y] (or (f1 x) (f1 y) nil))
                   ([x y z] (or (f1 x) (f1 y) (f1 z) nil))
                   ([x y z & args] (or (f1 x) (f1 y) (f1 z) (some f1 args)))))
           ([f1 f2] (fn
                      ([] nil)
                      ([x] (or (f1 x) (f2 x) nil))
                      ([x y] (or (f1 x) (f1 y) (f2 x) (f2 y) nil))
                      ([x y z] (or (f1 x) (f1 y) (f1 z) (f2 x) (f2 y) (f2 z) nil))
                      ([x y z & args] (let [tf (fn [f] (or (f x) (f y) (f z) (some f args)))]
                                        (or (tf f1) (tf f2))))))
           ([f1 f2 f3] (fn
                         ([] nil)
                         ([x] (or (f1 x) (f2 x) (f3 x) nil))
                         ([x y] (or (f1 x) (f1 y) (f2 x) (f2 y) (f3 x) (f3 y) nil))
                         ([x y z] (or (f1 x) (f1 y) (f1 z) (f2 x) (f2 y) (f2 z) (f3 x) (f3 y) (f3 z) nil))
                         ([x y z & args] (let [tf (fn [f] (or (f x) (f y) (f z) (some f args)))] (or (tf f1) (tf f2) (tf f3))))))
           ([f1 f2 f3 & fs] (fn
                              ([] nil)
                              ([x] (or (f1 x) (f2 x) (f3 x) (some (fn [f] (f x)) fs)))
                              ([x y] (or (f1 x) (f1 y) (f2 x) (f2 y) (f3 x) (f3 y) (some (fn [f] (or (f x) (f y))) fs)))
                              ([x y z] (or (f1 x) (f1 y) (f1 z) (f2 x) (f2 y) (f2 z) (f3 x) (f3 y) (f3 z) (some (fn [f] (or (f x) (f y) (f z))) fs)))
                              ([x y z & args] (let [tf (fn [f] (or (f x) (f y) (f z) (some f args)))] (or (tf f1) (tf f2) (tf f3) (some tf fs)))))))))
  (is (= (prettify-unroll (unroll-arities unroll-somef-spec))
         '(([] (cc/fn ([] nil) ([x] nil) ([x y] nil) ([x y z] nil) ([x y z & args] nil)))
           ([f1] (cc/fn
                   ([] nil)
                   ([x] (cc/or (f1 x) nil))
                   ([x y] (cc/or (f1 x) (f1 y) nil))
                   ([x y z] (cc/or (f1 x) (f1 y) (f1 z) nil))
                   ([x y z & args] (cc/or (f1 x) (f1 y) (f1 z) (cc/some f1 args)))))
           ([f1 f2] (cc/fn
                      ([] nil)
                      ([x] (cc/or (f1 x) (f2 x) nil))
                      ([x y] (cc/or (f1 x) (f1 y)
                                    (f2 x) (f2 y)
                                    nil))
                      ([x y z] (cc/or (f1 x) (f1 y) (f1 z)
                                      (f2 x) (f2 y) (f2 z)
                                      nil))
                      ([x y z & args] (cc/or (f1 x) (f1 y) (f1 z) (cc/some f1 args)
                                             (f2 x) (f2 y) (f2 z) (cc/some f2 args)))))
           ([f1 f2 f3] (cc/fn
                         ([] nil)
                         ([x] (cc/or (f1 x) (f2 x) (f3 x) nil))
                         ([x y] (cc/or (f1 x) (f1 y)
                                       (f2 x) (f2 y)
                                       (f3 x) (f3 y)
                                       nil))
                         ([x y z] (cc/or (f1 x) (f1 y) (f1 z)
                                         (f2 x) (f2 y) (f2 z)
                                         (f3 x) (f3 y) (f3 z)
                                         nil))
                         ([x y z & args] (cc/or (f1 x) (f1 y) (f1 z) (cc/some f1 args)
                                                (f2 x) (f2 y) (f2 z) (cc/some f2 args)
                                                (f3 x) (f3 y) (f3 z) (cc/some f3 args)))))
           ([f1 f2 f3 & fs] (cc/fn
                              ([] nil)
                              ([x] (cc/or (f1 x) (f2 x) (f3 x) (cc/some (cc/fn [f] (f x)) fs)))
                              ([x y] (cc/or (f1 x) (f1 y) (f2 x) (f2 y) (f3 x) (f3 y) (cc/some (cc/fn [f] (cc/or (f x) (f y))) fs)))
                              ([x y z] (cc/or (f1 x) (f1 y) (f1 z) (f2 x) (f2 y) (f2 z) (f3 x) (f3 y) (f3 z) (cc/some (cc/fn [f] (cc/or (f x) (f y) (f z))) fs)))
                              ([x y z & args] (cc/or (f1 x) (f1 y) (f1 z) (cc/some f1 args)
                                                     (f2 x) (f2 y) (f2 z) (cc/some f2 args)
                                                     (f3 x) (f3 y) (f3 z) (cc/some f3 args)
                                                     (cc/some (cc/fn [f] (cc/or (f x) (f y) (f z) (cc/some f args)))
                                                              fs)))))))))

(defunroll unroll-somef
  "Combines functions into a variable-arity disjunction.
  
  Definitionally equivalent to:

    (defn somef [& fs]
      (fn [& args] (some #(some % args) fs)))"
  {:arglists '([& fs])}
  unroll-somef-spec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/fnil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
(defn fnil
  "Takes a function f, and returns a function that calls f, replacing
  a nil first argument to f with the supplied value x. Higher arity
  versions can replace arguments in the second and third
  positions (y, z). Note that the function f can take any number of
  arguments, not just the one(s) being nil-patched."
  {:added "1.2"
   :static true}
  ([f x]
   (fn
     ([a] (f (if (nil? a) x a)))
     ([a b] (f (if (nil? a) x a) b))
     ([a b c] (f (if (nil? a) x a) b c))
     ([a b c & ds] (apply f (if (nil? a) x a) b c ds))))
  ([f x y]
   (fn
     ([a b] (f (if (nil? a) x a) (if (nil? b) y b)))
     ([a b c] (f (if (nil? a) x a) (if (nil? b) y b) c))
     ([a b c & ds] (apply f (if (nil? a) x a) (if (nil? b) y b) c ds))))
  ([f x y z]
   (fn
     ([a b] (f (if (nil? a) x a) (if (nil? b) y b)))
     ([a b c] (f (if (nil? a) x a) (if (nil? b) y b) (if (nil? c) z c)))
     ([a b c & ds] (apply f (if (nil? a) x a) (if (nil? b) y b) (if (nil? c) z c) ds)))))

(def unroll-fnil-spec
  {:argvs (uniformly-flowing-argvs
            {:arities (range 2 5)
             :fixed-names (cons 'f (single-char-syms-from \x))
             :rest-arity :skip})
   :unroll-arity (fn [{[f & xs] :fixed-args}]
                   `(fn ~@(unroll-arities
                            {:argvs (uniformly-flowing-argvs
                                      {:arities (range (if (= 1 (count xs)) 1 2)
                                                       4)
                                       :fixed-names (single-char-syms-from \a)
                                       :rest-name 'ds})
                             :unroll-arity (fn [{as :fixed-args ds :rest-arg}]
                                             (maybe-apply f
                                                          (map (fn [a x]
                                                                 (if x
                                                                   `(if (nil? ~a) ~x ~a)
                                                                   a))
                                                               as (concat xs (repeat nil)))
                                                          ds))})))})

(deftest unroll-fnil-spec-test
  (is (= (prettify-unroll (unroll-arities unroll-fnil-spec))
         '(([f x] (cc/fn
                    ([a] (f (if (cc/nil? a) x a)))
                    ([a b] (f (if (cc/nil? a) x a)
                              b))
                    ([a b c] (f (if (cc/nil? a) x a)
                                b
                                c))
                    ([a b c & ds] (cc/apply f
                                            (if (cc/nil? a) x a)
                                            b
                                            c
                                            ds))))
           ([f x y] (cc/fn
                      ([a b] (f (if (cc/nil? a) x a)
                                (if (cc/nil? b) y b)))
                      ([a b c] (f (if (cc/nil? a) x a)
                                  (if (cc/nil? b) y b)
                                  c))
                      ([a b c & ds] (cc/apply f
                                              (if (cc/nil? a) x a)
                                              (if (cc/nil? b) y b)
                                              c
                                              ds))))
           ([f x y z] (cc/fn
                        ([a b] (f (if (cc/nil? a) x a)
                                  (if (cc/nil? b) y b)))
                        ([a b c] (f (if (cc/nil? a) x a)
                                    (if (cc/nil? b) y b)
                                    (if (cc/nil? c) z c)))
                        ([a b c & ds] (cc/apply f
                                                (if (cc/nil? a) x a)
                                                (if (cc/nil? b) y b)
                                                (if (cc/nil? c) z c)
                                                ds))))))))

(defunroll unroll-fnil
  "Takes a function f, and returns a function that calls f, replacing
  a nil first argument to f with the supplied value x. Higher arity
  versions can replace arguments in the second and third
  positions (y, z). Note that the function f can take any number of
  arguments, not just the one(s) being nil-patched."
  {:added "1.2"
   :static true}
  unroll-fnil-spec)

(deftest unroll-fnil-test
  (is (= (-> #'unroll-fnil meta :arglists)
         (-> #'clojure.core/fnil meta :arglists)
         '([f x] [f x y] [f x y z])))
  (doseq [patched-args (map range (range 1 4))
          :let [defaults (map (fn [i] (+ 10 (* i i))) patched-args)]
          provide-nil (map set (comb/subsets patched-args))
          :let [unpatched (map (fn [i] (when-not (provide-nil i) (+ 10 i)))
                               patched-args)]
          i (range 10)]
    (is (= (apply (apply fnil + defaults) (concat unpatched (range i)))
           (apply (apply unroll-fnil + defaults) (concat unpatched (range i)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/binding-conveyor-fn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
(defn binding-conveyor-fn
  {:private true
   :added "1.3"}
  [f]
  (let [frame (clojure.lang.Var/cloneThreadBindingFrame)]
    (fn 
      ([]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f))
      ([x]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f x))
      ([x y]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f x y))
      ([x y z]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f x y z))
      ([x y z & args] 
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (apply f x y z args)))))

(def unroll-binding-conveyor-fn-spec
  {:argvs '[[f]]
   :unroll-arity (fn [{[f & xs] :fixed-args}]
                   (let [frame (gensym-pretty 'frame)]
                     `(let [~frame (clojure.lang.Var/cloneThreadBindingFrame)]
                        (fn ~@(unroll-arities
                                {:argvs (uniformly-flowing-argvs
                                          {:arities (range 4)
                                           :fixed-names (single-char-syms-from \x)
                                           :rest-name 'args})
                                 :unroll-arity (fn [{xs :fixed-args args :rest-arg}]
                                                 `(do (clojure.lang.Var/resetThreadBindingFrame ~frame)
                                                      ~(maybe-apply f xs args)))})))))})

(deftest unroll-binding-conveyor-fn-spec-test
  (is (= (prettify-unroll (unroll-arities unroll-binding-conveyor-fn-spec))
         '([f] (cc/let [frame (clojure.lang.Var/cloneThreadBindingFrame)]
                 (cc/fn
                   ([] (do (clojure.lang.Var/resetThreadBindingFrame frame)
                           (f)))
                   ([x] (do (clojure.lang.Var/resetThreadBindingFrame frame)
                            (f x)))
                   ([x y] (do (clojure.lang.Var/resetThreadBindingFrame frame)
                              (f x y)))
                   ([x y z] (do (clojure.lang.Var/resetThreadBindingFrame frame)
                                (f x y z)))
                   ([x y z & args] (do (clojure.lang.Var/resetThreadBindingFrame frame) (cc/apply f x y z args)))))))))

(defunroll unroll-binding-conveyor-fn
  "doc"
  {:private true
   :added "1.3"}
  unroll-binding-conveyor-fn-spec)

;;TODO test semantics
(deftest unroll-binding-conveyor-fn-test
  (is (= (-> #'unroll-binding-conveyor-fn meta :arglists)
         (-> #'clojure.core/binding-conveyor-fn meta :arglists)
         '([f]))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/swap!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
(defn swap!
  "Atomically swaps the value of atom to be:
  (apply f current-value-of-atom args). Note that f may be called
  multiple times, and thus should be free of side effects.  Returns
  the value that was swapped in."
  {:added "1.0"
   :static true}
  ([^clojure.lang.IAtom atom f] (.swap atom f))
  ([^clojure.lang.IAtom atom f x] (.swap atom f x))
  ([^clojure.lang.IAtom atom f x y] (.swap atom f x y))
  ([^clojure.lang.IAtom atom f x y & args] (.swap atom f x y args)))

(def unroll-swap!-spec
  {:argvs (uniformly-flowing-argvs
            {:arities (range 3)
             :leading-names [(with-meta 'atom {:tag 'clojure.lang.IAtom})
                             'f]
             :fixed-names (single-char-syms-from \x)
             :rest-name 'args})
   :unroll-arity (fn [{[atm f & xs] :fixed-args args :rest-arg}]
                   (assert ((if args = <=) (count xs) 2))
                   `(.swap ~atm ~f ~@xs ~@(some-> args list)))})

(deftest unroll-swap!-spec-test
  (is (= (prettify-unroll (unroll-arities unroll-swap!-spec))
         '(([atom f] (.swap atom f))
           ([atom f x] (.swap atom f x))
           ([atom f x y] (.swap atom f x y))
           ([atom f x y & args] (.swap atom f x y args))))))

(defunroll unroll-swap!
  "Atomically swaps the value of atom to be:
  (apply f current-value-of-atom args). Note that f may be called
  multiple times, and thus should be free of side effects.  Returns
  the value that was swapped in."
  {:added "1.0"
   :static true}
  unroll-swap!-spec)

;;TODO test semantics
(deftest unroll-swap!-test
  (is (= (-> #'unroll-swap! meta :arglists)
         (-> #'clojure.core/swap! meta :arglists)
         '([atom f]
           [atom f x]
           [atom f x y]
           [atom f x y & args])))
  (is (= (->> #'unroll-swap! meta :arglists (mapv (comp :tag meta first)))
         (->> #'clojure.core/swap! meta :arglists (mapv (comp :tag meta first)))
         (repeat 4 'clojure.lang.IAtom))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/swap-vals!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
(defn swap-vals!
  "Atomically swaps the value of atom to be:
  (apply f current-value-of-atom args). Note that f may be called
  multiple times, and thus should be free of side effects.
  Returns [old new], the value of the atom before and after the swap."
  {:added "1.9"}
  (^clojure.lang.IPersistentVector [^clojure.lang.IAtom2 atom f] (.swapVals atom f))
  (^clojure.lang.IPersistentVector [^clojure.lang.IAtom2 atom f x] (.swapVals atom f x))
  (^clojure.lang.IPersistentVector [^clojure.lang.IAtom2 atom f x y] (.swapVals atom f x y))
  (^clojure.lang.IPersistentVector [^clojure.lang.IAtom2 atom f x y & args] (.swapVals atom f x y args)))

(def unroll-swap-vals!-spec
  {:argvs (mapv
            #(with-meta % {:tag 'clojure.lang.IPersistentVector})
            (uniformly-flowing-argvs
              {:arities (range 3)
               :leading-names [(with-meta 'atom {:tag 'clojure.lang.IAtom2})
                               'f]
               :fixed-names (single-char-syms-from \x)
               :rest-name 'args}))
   :unroll-arity (fn [{[atm f & xs] :fixed-args args :rest-arg}]
                   (assert ((if args = <=) (count xs) 2))
                   `(.swapVals ~atm ~f ~@xs ~@(some-> args list)))})

(deftest unroll-swap-vals!-spec-test
  (is (= (prettify-unroll (unroll-arities unroll-swap-vals!-spec))
         '(([atom f] (.swapVals atom f))
           ([atom f x] (.swapVals atom f x))
           ([atom f x y] (.swapVals atom f x y))
           ([atom f x y & args] (.swapVals atom f x y args))))))
 
(defunroll unroll-swap-vals!
  "Atomically swaps the value of atom to be:
  (apply f current-value-of-atom args). Note that f may be called
  multiple times, and thus should be free of side effects.
  Returns [old new], the value of the atom before and after the swap."
  {:added "1.9"}
  unroll-swap-vals!-spec)

;;TODO test semantics
(deftest unroll-swap-vals!-test
  (is (= (-> #'unroll-swap-vals! meta :arglists)
         (-> #'clojure.core/swap-vals! meta :arglists)
         '([atom f]
           [atom f x]
           [atom f x y]
           [atom f x y & args])))
  (is (= (->> #'unroll-swap-vals! meta :arglists (mapv (comp :tag meta first)))
         (->> #'clojure.core/swap-vals! meta :arglists (mapv (comp :tag meta first)))
         (repeat 4 'clojure.lang.IAtom2)))
  (is (= (->> #'unroll-swap-vals! meta :arglists (mapv (comp :tag meta)))
         (->> #'clojure.core/swap-vals! meta :arglists (mapv (comp :tag meta)))
         (repeat 4 'clojure.lang.IPersistentVector))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
(defn update
  "'Updates' a value in an associative structure, where k is a
  key and f is a function that will take the old value
  and any supplied args and return the new value, and returns a new
  structure.  If the key does not exist, nil is passed as the old value."
  {:added "1.7"
   :static true}
  ([m k f]
   (assoc m k (f (get m k))))
  ([m k f x]
   (assoc m k (f (get m k) x)))
  ([m k f x y]
   (assoc m k (f (get m k) x y)))
  ([m k f x y z]
   (assoc m k (f (get m k) x y z)))
  ([m k f x y z & more]
   (assoc m k (apply f (get m k) x y z more))))

(def unroll-update-spec
  {:argvs (uniformly-flowing-argvs
            {:arities (range 4)
             :leading-names ['m 'k 'f]
             :fixed-names (single-char-syms-from \x)
             :rest-name 'more})
   :unroll-arity (fn [{[m k f & xs] :fixed-args args :rest-arg}]
                   `(assoc ~m ~k ~(maybe-apply f (list* `(get ~m ~k) xs) args)))})

(deftest unroll-update-spec-test
  (is (= (prettify-unroll (unroll-arities unroll-update-spec))
         '(([m k f] (cc/assoc m k (f (cc/get m k))))
           ([m k f x] (cc/assoc m k (f (cc/get m k) x)))
           ([m k f x y] (cc/assoc m k (f (cc/get m k) x y)))
           ([m k f x y z] (cc/assoc m k (f (cc/get m k) x y z)))
           ([m k f x y z & more] (cc/assoc m k (cc/apply f (cc/get m k) x y z more)))))))

(defunroll unroll-update
  "'Updates' a value in an associative structure, where k is a
  key and f is a function that will take the old value
  and any supplied args and return the new value, and returns a new
  structure.  If the key does not exist, nil is passed as the old value."
  {:added "1.7"
   :static true}
  unroll-update-spec)

(deftest unroll-update-test
  (is (= (-> #'unroll-update meta :arglists)
         (-> #'clojure.core/update meta :arglists)
         '([m k f] [m k f x] [m k f x y] [m k f x y z] [m k f x y z & more])))
  (dotimes [i 10]
    (is (= (apply update {:a 1} :a + (range i))
           (apply unroll-update {:a 1} :a + (range i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/complement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
(defn complement
  "Takes a fn f and returns a fn that takes the same arguments as f,
  has the same effects, if any, and returns the opposite truth value."
  {:added "1.0"
   :static true}
  [f] 
  (fn 
    ([] (not (f)))
    ([x] (not (f x)))
    ([x y] (not (f x y)))
    ([x y & zs] (not (apply f x y zs)))))

(def unroll-complement-spec
  {:argvs '[[f]]
   :unroll-arity (fn [{[f] :fixed-args}]
                   `(fn ~@(unroll-arities
                            {:argvs (uniformly-flowing-argvs
                                      {:arities (range 4)
                                       :fixed-names (single-char-syms-from \x)
                                       :rest-name 'zs})
                             :unroll-arity (fn [{xs :fixed-args zs :rest-arg}]
                                             `(not ~(maybe-apply f xs zs)))})))})

(deftest unroll-complement-spec-test
  (is (= (prettify-unroll (unroll-arities unroll-complement-spec))
         '([f] (cc/fn
                 ([] (cc/not (f)))
                 ([x] (cc/not (f x)))
                 ([x y] (cc/not (f x y)))
                 ([x y z] (cc/not (f x y z)))
                 ([x y z & zs] (cc/not (cc/apply f x y z zs))))))))

(defunroll unroll-complement
  "Takes a fn f and returns a fn that takes the same arguments as f,
  has the same effects, if any, and returns the opposite truth value."
  {:added "1.0"
   :static true}
  unroll-complement-spec)

(deftest unroll-complement-test
  (is (= (-> #'unroll-complement meta :arglists)
         (-> #'clojure.core/complement meta :arglists)
         '([f])))
  (dotimes [i 10]
    (is (= (apply (complement #(even? (apply + %&))) (range i))
           (apply (unroll-complement #(even? (apply + %&))) (range i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/constantly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
(defn constantly
  "Returns a function that takes any number of arguments and returns x."
  {:added "1.0"
   :static true}
  [x] (fn [& args] x))

(defn unroll-constantly-spec*
  ([] (unroll-constantly-spec* {}))
  ([{:keys [size] :or {size 0}}]
   {:argvs '[[x]]
    :unroll-arity (fn [{[x] :fixed-args}]
                    `(fn ~@(unroll-arities
                             {:argvs (uniformly-flowing-argvs
                                       {:arities (range size)
                                        :fixed-names (map #(symbol (str 'a %)) (next (range)))
                                        :rest-name 'args})
                              :unroll-arity (fn [_] x)})))}))

(def unroll-constantly-spec (unroll-constantly-spec*))

(deftest unroll-constantly-spec-test
  (is (= (prettify-unroll (unroll-arities (unroll-constantly-spec* {:size 4})))
         '([x] (cc/fn
                 ([] x)
                 ([a1] x)
                 ([a1 a2] x)
                 ([a1 a2 a3] x)
                 ([a1 a2 a3 & args] x)))))
  (is (= (prettify-unroll (unroll-arities unroll-constantly-spec))
         '([x] (cc/fn [& args] x)))))

(defunroll unroll-constantly
  "Returns a function that takes any number of arguments and returns x."
  {:added "1.0"
   :static true}
  unroll-constantly-spec)

(deftest unroll-constantly-test
  (is (= (-> #'unroll-constantly meta :arglists)
         (-> #'clojure.core/constantly meta :arglists)
         '([x])))
  (dotimes [i 10]
    (is (= (apply (constantly i) (range i))
           (apply (unroll-constantly i) (range i))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
(defn map
  "Returns a lazy sequence consisting of the result of applying f to
  the set of first items of each coll, followed by applying f to the
  set of second items in each coll, until any one of the colls is
  exhausted.  Any remaining items in other colls are ignored. Function
  f should accept number-of-colls arguments. Returns a transducer when
  no collection is provided."
  {:added "1.0"
   :static true}
  ([f]
    (fn [rf]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
           (rf result (f input)))
        ([result input & inputs]
           (rf result (apply f input inputs))))))
  ([f coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (if (chunked-seq? s)
        (let [c (chunk-first s)
              size (int (count c))
              b (chunk-buffer size)]
          (dotimes [i size]
              (chunk-append b (f (.nth c i))))
          (chunk-cons (chunk b) (map f (chunk-rest s))))
        (cons (f (first s)) (map f (rest s)))))))
  ([f c1 c2]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2)]
      (when (and s1 s2)
        (cons (f (first s1) (first s2))
              (map f (rest s1) (rest s2)))))))
  ([f c1 c2 c3]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2) s3 (seq c3)]
      (when (and  s1 s2 s3)
        (cons (f (first s1) (first s2) (first s3))
              (map f (rest s1) (rest s2) (rest s3)))))))
  ([f c1 c2 c3 & colls]
   (let [step (fn step [cs]
                 (lazy-seq
                  (let [ss (map seq cs)]
                    (when (every? identity ss)
                      (cons (map first ss) (step (map rest ss)))))))]
     (map #(apply f %) (step (conj colls c3 c2 c1))))))

(def unroll-map-spec
  {:argvs (list* '[f] '[f coll]
                 (uniformly-flowing-argvs
                   {:arities (range 2 4)
                    :leading-names '[f]
                    :fixed-names (map #(symbol (str 'c %)) (next (range)))
                    :rest-name 'colls}))
   :unroll-arity (fn [{:keys [this] [f & cxs] :fixed-args colls :rest-arg}]
                   (assert (and this f))
                   (cond
                     ;; transducer
                     (and (not cxs) (not colls))
                     (let [rf (gensym-pretty 'rf)]
                       `(fn [~rf]
                          (fn ~@(unroll-arities
                                  {:argvs (uniformly-flowing-argvs
                                            {:arities (range 3)
                                             :fixed-names (cons 'result (map #(symbol (str 'input %)) (next (range))))
                                             :rest-name 'inputs})
                                   :unroll-arity (fn [{[result & inputxs] :fixed-args inputs :rest-arg}]
                                                   `(~rf ~@(some-> result list)
                                                         ~@(when (and result (or inputxs inputs))
                                                             [(maybe-apply f inputxs inputs)])))}))))

                     ;; variable
                     colls
                     (let [[step cs ss xs] (map gensym-pretty '[step cs ss xs])]
                       `(let [~step (fn ~step [~cs]
                                      (lazy-seq
                                        (let [~ss (~this seq ~cs)]
                                          (when (every? identity ~ss)
                                            (cons (~this first ~ss) (~step (~this rest ~ss)))))))]
                          (~this (fn [~xs] (apply ~f ~xs)) (~step ~(maybe-conj colls (reverse cxs))))))

                     ;; fixed
                     :else
                     (let [sxs (map-indexed (fn [i _] (gensym-pretty (str 's (inc i)))) cxs)
                           non-chunked-case `(cons (~f ~@(map #(list `first %) sxs))
                                                   (~this ~f ~@(map #(list `rest %) sxs)))]
                       `(lazy-seq
                          (let [~@(mapcat (fn [s c] [s `(seq ~c)]) sxs cxs)]
                            (when ~(maybe-and sxs)
                              ~(if (= 1 (count cxs))
                                 (let [[coll] cxs
                                       [s] sxs
                                       [c size b i] (map gensym-pretty '[c size b i])]
                                   `(if (chunked-seq? ~s)
                                      (let [~c (chunk-first ~s)
                                            ~size (int (count ~c))
                                            ~b (chunk-buffer ~size)]
                                        (dotimes [~i ~size]
                                          (chunk-append ~b (~f (.nth ~c ~i))))
                                        (chunk-cons (chunk ~b) (~this ~f (chunk-rest ~s))))
                                      ~non-chunked-case))
                                 non-chunked-case)))))))})

(deftest unroll-map-spec-test
  (is (= (prettify-unroll (unroll-arities (assoc unroll-map-spec :this 'this/map)))
         '(([f] (cc/fn [rf]
                  (cc/fn
                    ([] (rf))
                    ([result] (rf result))
                    ([result input1] (rf result (f input1)))
                    ([result input1 & inputs] (rf result (cc/apply f input1 inputs))))))
           ([f coll] (cc/lazy-seq
                       (cc/let [s1 (cc/seq coll)]
                         (cc/when s1
                           (if (cc/chunked-seq? s1)
                             (cc/let [c (cc/chunk-first s1) size (cc/int (cc/count c)) b (cc/chunk-buffer size)]
                               (cc/dotimes [i size] (cc/chunk-append b (f (.nth c i))))
                               (cc/chunk-cons (cc/chunk b)
                                              (this/map f (cc/chunk-rest s1))))
                             (cc/cons (f (cc/first s1))
                                      (this/map f (cc/rest s1))))))))
           ([f c1 c2] (cc/lazy-seq
                        (cc/let [s1 (cc/seq c1) s2 (cc/seq c2)]
                          (cc/when (cc/and s1 s2)
                            (cc/cons (f (cc/first s1) (cc/first s2))
                                     (this/map f (cc/rest s1) (cc/rest s2)))))))
           ([f c1 c2 c3] (cc/lazy-seq
                           (cc/let [s1 (cc/seq c1) s2 (cc/seq c2) s3 (cc/seq c3)]
                             (cc/when (cc/and s1 s2 s3)
                               (cc/cons (f (cc/first s1) (cc/first s2) (cc/first s3))
                                        (this/map f (cc/rest s1) (cc/rest s2) (cc/rest s3)))))))
           ([f c1 c2 c3 & colls] (cc/let [step (cc/fn step [cs]
                                                 (cc/lazy-seq
                                                   (cc/let [ss (this/map cc/seq cs)]
                                                     (cc/when (cc/every? cc/identity ss)
                                                       (cc/cons (this/map cc/first ss)
                                                                (step (this/map cc/rest ss)))))))]
                                   (this/map (cc/fn [xs] (cc/apply f xs)) (step (cc/conj colls c3 c2 c1)))))))))

(defunroll unroll-map
  "Returns a lazy sequence consisting of the result of applying f to
  the set of first items of each coll, followed by applying f to the
  set of second items in each coll, until any one of the colls is
  exhausted.  Any remaining items in other colls are ignored. Function
  f should accept number-of-colls arguments. Returns a transducer when
  no collection is provided."
  {:added "1.0"
   :static true}
  unroll-map-spec)

(deftest unroll-map-test
  (is (= (-> #'unroll-map meta :arglists)
         (-> #'clojure.core/map meta :arglists)
         '([f] [f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls])))
  (dotimes [i 10]
    ;;TODO test multi-arity transducer
    (is (= (into [] (map inc) (range i))
           (into [] (unroll-map inc) (range i))))
    (is (= (apply map + (repeat (inc i) (range i)))
           (apply unroll-map + (repeat (inc i) (range i)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/interleave
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
(defn interleave
  "Returns a lazy seq of the first item in each coll, then the second etc."
  {:added "1.0"
   :static true}
  ([] ())
  ([c1] (lazy-seq c1))
  ([c1 c2]
     (lazy-seq
      (let [s1 (seq c1) s2 (seq c2)]
        (when (and s1 s2)
          (cons (first s1) (cons (first s2) 
                                 (interleave (rest s1) (rest s2))))))))
  ([c1 c2 & colls] 
     (lazy-seq 
      (let [ss (map seq (conj colls c2 c1))]
        (when (every? identity ss)
          (concat (map first ss) (apply interleave (map rest ss))))))))

(def unroll-interleave-spec
  {:argvs (uniformly-flowing-argvs
            {:arities (range 3)
             :fixed-names (map #(symbol (str 'c %)) (next (range)))
             :rest-name 'colls})
   :unroll-arity (fn [{:keys [this] cxs :fixed-args colls :rest-arg}]
                   (case (count cxs)
                     0 ()
                     1 `(lazy-seq ~(first cxs))
                     (if colls
                       (let [ss (gensym-pretty 'ss)]
                         `(lazy-seq 
                            (let [~ss (map seq ~(maybe-conj colls (reverse cxs)))]
                              (when (every? identity ~ss)
                                (concat (map first ~ss) (apply ~this (map rest ~ss)))))))
                       (let [sxs (map-indexed (fn [i _] (gensym-pretty (str 's (inc i)))) cxs)]
                         `(lazy-seq
                            (let [~@(mapcat (fn [s c] [s `(seq ~c)]) sxs cxs)]
                              (when ~(maybe-and sxs)
                                ~(reduce (fn [acc s]
                                           `(cons (first ~s) ~acc))
                                         `(~this ~@(map #(list `rest %) sxs))
                                         (reverse sxs)))))))))})

(deftest unroll-interleave-spec-test
  (is (= (prettify-unroll (unroll-arities (assoc unroll-interleave-spec :this 'this/interleave)))
         '(([] ())
           ([c1] (cc/lazy-seq c1))
           ([c1 c2] (cc/lazy-seq
                      (cc/let [s1 (cc/seq c1) s2 (cc/seq c2)]
                        (cc/when (cc/and s1 s2)
                          (cc/cons (cc/first s1)
                                   (cc/cons (cc/first s2)
                                            (this/interleave (cc/rest s1) (cc/rest s2))))))))
           ([c1 c2 & colls] (cc/lazy-seq
                              (cc/let [ss (cc/map cc/seq (cc/conj colls c2 c1))]
                                (cc/when (cc/every? cc/identity ss)
                                  (cc/concat (cc/map cc/first ss)
                                             (cc/apply this/interleave (cc/map cc/rest ss)))))))))))

(defunroll unroll-interleave
  "Returns a lazy seq of the first item in each coll, then the second etc."
  {:added "1.0"
   :static true}
  unroll-interleave-spec)

(deftest unroll-interleave-test
  (is (= (-> #'unroll-interleave meta :arglists)
         (-> #'clojure.core/interleave meta :arglists)
         '([] [c1] [c1 c2] [c1 c2 & colls])))
  (dotimes [i 10]
    (is (= (apply interleave (map (fn [i] (range i (+ 10 i))) (range i)))
           (apply unroll-interleave (map (fn [i] (range i (+ 10 i))) (range i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/memoize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
(defn memoize
  "Returns a memoized version of a referentially transparent function. The
  memoized version of the function keeps a cache of the mapping from arguments
  to results and, when calls with the same arguments are repeated often, has
  higher performance at the expense of higher memory use."
  {:added "1.0"
   :static true}
  [f]
  (let [mem (atom {})]
    (fn [& args]
      (if-let [e (find @mem args)]
        (val e)
        (let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret)))))

(def unroll-memoize-spec
  {:argvs '[[f]]
   :unroll-arity (fn [{[f] :fixed-args}]
                   (let [[mem e ret k] (map gensym-pretty '[mem e ret k])]
                     `(let [~mem (atom {})]
                        (fn ~@(unroll-arities
                                {:argvs (uniformly-flowing-argvs
                                          {:arities (range 4)
                                           :fixed-names (single-char-syms-from \x)
                                           :rest-name 'args})
                                 :unroll-arity (fn [{xs :fixed-args args :rest-arg}]
                                                 `(let [~k ~(maybe-list* xs args)]
                                                    (if-let [~e (find @~mem ~k)]
                                                      (val ~e)
                                                      (let [~ret ~(maybe-apply f xs args)]
                                                        (swap! ~mem assoc ~k ~ret)
                                                        ~ret))))})))))})

(deftest unroll-memoize-spec-test
  (is (= (prettify-unroll (unroll-arities unroll-memoize-spec))
         '([f] (cc/let [mem (cc/atom {})]
                 (cc/fn
                   ([] (cc/let [k ()]
                         (cc/if-let [e (cc/find (cc/deref mem) k)]
                           (cc/val e)
                           (cc/let [ret (f)]
                             (cc/swap! mem cc/assoc k ret)
                             ret))))
                   ([x] (cc/let [k (cc/list x)]
                          (cc/if-let [e (cc/find (cc/deref mem) k)]
                            (cc/val e)
                            (cc/let [ret (f x)]
                              (cc/swap! mem cc/assoc k ret)
                              ret))))
                   ([x y] (cc/let [k (cc/list x y)]
                            (cc/if-let [e (cc/find (cc/deref mem) k)]
                              (cc/val e)
                              (cc/let [ret (f x y)]
                                (cc/swap! mem cc/assoc k ret)
                                ret))))
                   ([x y z] (cc/let [k (cc/list x y z)]
                              (cc/if-let [e (cc/find (cc/deref mem) k)]
                                (cc/val e)
                                (cc/let [ret (f x y z)]
                                  (cc/swap! mem cc/assoc k ret)
                                  ret))))
                   ([x y z & args] (cc/let [k (cc/list* x y z args)]
                                     (cc/if-let [e (cc/find (cc/deref mem) k)]
                                       (cc/val e)
                                       (cc/let [ret (cc/apply f x y z args)]
                                         (cc/swap! mem cc/assoc k ret)
                                         ret))))))))))

(defunroll unroll-memoize
  "Returns a memoized version of a referentially transparent function. The
  memoized version of the function keeps a cache of the mapping from arguments
  to results and, when calls with the same arguments are repeated often, has
  higher performance at the expense of higher memory use."
  {:added "1.0"
   :static true}
  unroll-memoize-spec)

(deftest unroll-memoize-test
  (is (= (-> #'unroll-memoize meta :arglists)
         '([f])))
  (dotimes [i 10]
    (let [mem-atom (atom 0)
          mem (memoize (fn [& args]
                         (swap! mem-atom inc)
                         (apply + args)))
          umem-atom (atom 0)
          umem (unroll-memoize (fn [& args]
                                 (swap! umem-atom inc)
                                 (apply + args)))]
      (dotimes [_ 5]
        (is (= (apply mem (range i))
               (apply umem (range i)))))
      (is (= 1 @mem-atom @umem-atom)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/mapv
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
(defn mapv
  "Returns a vector consisting of the result of applying f to the
  set of first items of each coll, followed by applying f to the set
  of second items in each coll, until any one of the colls is
  exhausted.  Any remaining items in other colls are ignored. Function
  f should accept number-of-colls arguments."
  {:added "1.4"
   :static true}
  ([f coll]
     (-> (reduce (fn [v o] (conj! v (f o))) (transient []) coll)
         persistent!))
  ([f c1 c2]
     (into [] (map f c1 c2)))
  ([f c1 c2 c3]
     (into [] (map f c1 c2 c3)))
  ([f c1 c2 c3 & colls]
     (into [] (apply map f c1 c2 c3 colls))))

(def unroll-mapv-spec
  {:argvs (cons '[f coll]
                (uniformly-flowing-argvs
                  {:arities (range 2 4)
                   :leading-names ['f]
                   :fixed-names (map #(symbol (str 'c %)) (next (range)))
                   :rest-name 'colls}))
   :unroll-arity (fn [{[f & cxs] :fixed-args colls :rest-arg}]
                   (if (and (= 1 (count cxs)) (not colls))
                     (let [[coll] cxs
                           [v o] (map gensym-pretty '[v o])]
                       `(-> (reduce (fn [~v ~o] (conj! ~v (~f ~o))) (transient []) ~coll)
                            persistent!))
                     `(into [] ~(maybe-apply `map (cons f cxs) colls))))})

(deftest unroll-mapv-spec-test
  (is (= (prettify-unroll (unroll-arities unroll-mapv-spec))
         '(([f coll] (cc/-> (cc/reduce (cc/fn [v o] (cc/conj! v (f o))) (cc/transient []) coll)
                            cc/persistent!))
           ([f c1 c2] (cc/into [] (cc/map f c1 c2)))
           ([f c1 c2 c3] (cc/into [] (cc/map f c1 c2 c3)))
           ([f c1 c2 c3 & colls] (cc/into [] (cc/apply cc/map f c1 c2 c3 colls)))))))

(defunroll unroll-mapv
  "Returns a vector consisting of the result of applying f to the
  set of first items of each coll, followed by applying f to the set
  of second items in each coll, until any one of the colls is
  exhausted.  Any remaining items in other colls are ignored. Function
  f should accept number-of-colls arguments."
  {:added "1.4"
   :static true}
  unroll-mapv-spec)

(deftest unroll-mapv-test
  (is (= (-> #'unroll-mapv meta :arglists)
         (-> #'clojure.core/mapv meta :arglists)
         '([f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls])))
  (dotimes [i 10]
    (is (= (apply mapv + (repeat (inc i) (range i)))
           (apply unroll-mapv + (repeat (inc i) (range i)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/merge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
(defn merge
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping from
  the latter (left-to-right) will be the mapping in the result."
  {:added "1.0"
   :static true}
  [& maps]
  (when (some identity maps)
    (reduce1 #(conj (or %1 {}) %2) maps)))

(defn unroll-merge-spec*
  ([] (unroll-merge-spec* {}))
  ([{:keys [size] :or {size 5}}]
   {:argvs (uniformly-flowing-argvs
             {:arities (range size)
              :fixed-names (map #(symbol (str 'm %)) (next (range)))
              :rest-name 'maps})
    :unroll-arity (fn [{ms :fixed-args maps :rest-arg}]
                    (if (and (= 1 (count ms))
                             (not maps))
                      `(or ~(first ms) nil)
                      (maybe-when (maybe-or (cond-> ms maps (conj `(some identity ~maps))))
                                  (maybe-reduce `conj
                                                (if (seq ms)
                                                  (maybe-> `(or ~(first ms) {})
                                                           (map (fn [m] `(conj ~m)) (next ms)))
                                                  `(or (first ~maps) {}))
                                                (when maps
                                                  (if (seq ms)
                                                    maps
                                                    `(rest ~maps)))))))}))

(def unroll-merge-spec (unroll-merge-spec*))

(deftest unroll-merge-spec-test
  (is (= (prettify-unroll (unroll-arities (unroll-merge-spec* {:size 0})))
         '([& maps] (cc/when (cc/some cc/identity maps)
                      (cc/reduce cc/conj (cc/or (cc/first maps) {}) (cc/rest maps))))))
  (is (= (prettify-unroll (unroll-arities (unroll-merge-spec* {:size 1})))
         '(([] nil)
           ([& maps] (cc/when (cc/some cc/identity maps)
                       (cc/reduce cc/conj (cc/or (cc/first maps) {}) (cc/rest maps)))))))
  (is (= (prettify-unroll (unroll-arities (unroll-merge-spec* {:size 2})))
         '(([] nil)
           ([m1] (cc/or m1 nil))
           ([m1 & maps] (cc/when (cc/or m1 (cc/some cc/identity maps))
                          (cc/reduce cc/conj (cc/or m1 {}) maps))))))
  (is (= (prettify-unroll (unroll-arities (unroll-merge-spec* {:size 3})))

         '(([] nil)
           ([m1] (cc/or m1 nil))
           ([m1 m2] (cc/when (cc/or m1 m2)
                      (cc/-> (cc/or m1 {})
                             (cc/conj m2))))
           ([m1 m2 & maps] (cc/when (cc/or m1 m2 (cc/some cc/identity maps))
                             (cc/reduce cc/conj
                                        (cc/-> (cc/or m1 {}) (cc/conj m2))
                                        maps))))))
  (is (= (prettify-unroll (unroll-arities unroll-merge-spec))
         '(([] nil)
           ([m1] (cc/or m1 nil))
           ([m1 m2] (cc/when (cc/or m1 m2)
                      (cc/-> (cc/or m1 {})
                             (cc/conj m2))))
           ([m1 m2 m3] (cc/when (cc/or m1 m2 m3)
                         (cc/-> (cc/or m1 {})
                                (cc/conj m2)
                                (cc/conj m3))))
           ([m1 m2 m3 m4] (cc/when (cc/or m1 m2 m3 m4)
                            (cc/-> (cc/or m1 {})
                                   (cc/conj m2)
                                   (cc/conj m3)
                                   (cc/conj m4))))
           ([m1 m2 m3 m4 & maps] (cc/when (cc/or m1 m2 m3 m4 (cc/some cc/identity maps))
                                   (cc/reduce cc/conj
                                              (cc/-> (cc/or m1 {})
                                                     (cc/conj m2)
                                                     (cc/conj m3)
                                                     (cc/conj m4))
                                              maps)))))))

(defunroll unroll-merge
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping from
  the latter (left-to-right) will be the mapping in the result."
  {:added "1.0"
   :static true}
  unroll-merge-spec)

(deftest unroll-merge-test
  (is (= (-> #'unroll-merge meta :arglists)
         '([] [m1] [m1 m2] [m1 m2 m3] [m1 m2 m3 m4] [m1 m2 m3 m4 & maps])))
  (doseq [i (range 10)
          provide-nil (map set (comb/subsets (range i)))
          :let [args (map (fn [i] (when-not (provide-nil i) {:a i}))
                          (range i))]]
    (is (= (apply merge args)
           (apply unroll-merge args)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/merge-with
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO
#_
(defn merge-with
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping(s)
  from the latter (left-to-right) will be combined with the mapping in
  the result by calling (f val-in-result val-in-latter)."
  {:added "1.0"
   :static true}
  [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
                        (let [k (key e) v (val e)]
                          (if (contains? m k)
                            (assoc m k (f (get m k) v))
                            (assoc m k v))))
          merge2 (fn [m1 m2]
                   (reduce1 merge-entry (or m1 {}) (seq m2)))]
      (reduce1 merge2 maps))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/dissoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO
#_
(defn dissoc
  "dissoc[iate]. Returns a new map of the same (hashed/sorted) type,
  that does not contain a mapping for key(s)."
  {:added "1.0"
   :static true}
  ([map] map)
  ([map key]
   (. clojure.lang.RT (dissoc map key)))
  ([map key & ks]
   (let [ret (dissoc map key)]
     (if ks
       (recur ret (first ks) (next ks))
       ret))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/disj
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO
#_
(defn disj
  "disj[oin]. Returns a new set of the same (hashed/sorted) type, that
  does not contain key(s)."
  {:added "1.0"
   :static true}
  ([set] set)
  ([^clojure.lang.IPersistentSet set key]
   (when set
     (. set (disjoin key))))
  ([set key & ks]
   (when set
     (let [ret (disj set key)]
       (if ks
         (recur ret (first ks) (next ks))
         ret)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/with-bindings*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO
#_
(defn with-bindings*
  "Takes a map of Var/value pairs. Installs for the given Vars the associated
  values as thread-local bindings. Then calls f with the supplied arguments.
  Pops the installed bindings after f returned. Returns whatever f returns."
  {:added "1.1"
   :static true}
  [binding-map f & args]
  (push-thread-bindings binding-map)
  (try
    (apply f args)
    (finally
      (pop-thread-bindings))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/bound-fn*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO
#_
(defn bound-fn*
  "Returns a function, which will install the same bindings in effect as in
  the thread at the time bound-fn* was called and then call f with any given
  arguments. This may be used to define a helper function which runs on a
  different thread, but needs the same bindings in place."
  {:added "1.1"
   :static true}
  [f]
  (let [bindings (get-thread-bindings)]
    (fn [& args]
      (apply with-bindings* bindings f args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/mapcat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
(defn mapcat
  "Returns the result of applying concat to the result of applying map
  to f and colls.  Thus function f should return a collection. Returns
  a transducer when no collections are provided"
  {:added "1.0"
   :static true}
  ([f] (comp (map f) cat))
  ([f & colls]
     (apply concat (apply map f colls))))

(defn unroll-mapcat-spec*
  ([] (unroll-mapcat-spec* {}))
  ([{:keys [size] :or {size 0}}]
   {:argvs (uniformly-flowing-argvs
             {:arities (range (inc size))
              :leading-names '[f]
              :fixed-names (map #(symbol (str 'c %)) (next (range)))
              :rest-name 'colls})
    :unroll-arity (fn [{[f & cxs] :fixed-args colls :rest-arg}]
                    (assert f)
                    (cond
                      (and (not cxs) (not colls)) `(comp (map ~f) cat)
                      :else `(apply concat
                                    ~(if colls
                                       `(apply map ~f ~@cxs ~colls)
                                       `(map ~f ~@cxs)))))}))

(def unroll-mapcat-spec (unroll-mapcat-spec*))

(deftest unroll-mapcat-spec-test
  (is (= (prettify-unroll (unroll-arities (unroll-mapcat-spec* {:size 1})))
         '(([f] (cc/comp (cc/map f) cc/cat))
           ([f c1] (cc/apply cc/concat (cc/map f c1)))
           ([f c1 & colls] (cc/apply cc/concat (cc/apply cc/map f c1 colls))))))
  (is (= (prettify-unroll (unroll-arities (unroll-mapcat-spec* {:size 2})))
         '(([f] (cc/comp (cc/map f) cc/cat))
           ([f c1] (cc/apply cc/concat (cc/map f c1)))
           ([f c1 c2] (cc/apply cc/concat (cc/map f c1 c2)))
           ([f c1 c2 & colls] (cc/apply cc/concat (cc/apply cc/map f c1 c2 colls))))))
  (is (= (prettify-unroll (unroll-arities unroll-mapcat-spec))
         '(([f] (cc/comp (cc/map f) cc/cat)) ([f & colls] (cc/apply cc/concat (cc/apply cc/map f colls)))))))

(defunroll unroll-mapcat
  "Returns the result of applying concat to the result of applying map
  to f and colls.  Thus function f should return a collection. Returns
  a transducer when no collections are provided"
  {:added "1.0"
   :static true}
  unroll-mapcat-spec)

(deftest unroll-mapcat-test
  (is (= (-> #'unroll-mapcat meta :arglists)
         (-> #'clojure.core/mapcat meta :arglists)
         '([f] [f & colls])))
  (dotimes [i 10]
    (is (= (into [] (mapcat vector) (range i))
           (into [] (unroll-mapcat vector) (range i))))
    (is (= (apply mapcat vector (repeat (inc i) (range i)))
           (apply unroll-mapcat vector (repeat (inc i) (range i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; medley.core/assoc-some
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn assoc-some
  "Associates a key k, with a value v in a map m, if and only if v is not nil."
  ([m k v]
   (if (nil? v) m (assoc m k v)))
  ([m k v & kvs]
   (reduce (fn [m [k v]] (assoc-some m k v))
           (assoc-some m k v)
           (partition 2 kvs))))

(defn assoc-some-spec*
  ([] (assoc-some-spec* {}))
  ([{:keys [size]
     :or {size 4}}]
   (assert (nat-int? size))
   {:argvs (uniformly-flowing-argvs
             {:arities (map #(* % 2) (range 1 size))
              :leading-names '[m]
              :fixed-names (mapcat (fn [i] [(symbol (str 'k i))
                                            (symbol (str 'v i))])
                                   (next (range)))
              :rest-name 'kvs})
    :unroll-arity (fn [{[m & [k v :as fixed-kvs]] :fixed-args kvs :rest-arg assoc-some :this}]
                    (assert assoc-some)
                    (let [fixed `(cond-> ~m
                                   ~@(mapcat (fn [[k v]]
                                               [`(some? ~v) `(assoc ~k ~v)])
                                             (partition 2 fixed-kvs)))]
                      (if kvs
                        `(reduce (fn [~m [~k ~v]] (~assoc-some ~m ~k ~v))
                                 (~assoc-some ~m ~@fixed-kvs)
                                 (partition 2 ~kvs))
                        fixed)))}))

(def assoc-some-spec (assoc-some-spec*))

(deftest unroll-assoc-some-spec-test
  (is (= (prettify-unroll (unroll-arities (assoc assoc-some-spec :this 'this/assoc-some)))
         '(([m k1 v1] (cc/cond-> m (cc/some? v1) (cc/assoc k1 v1)))
           ([m k1 v1 k2 v2] (cc/cond-> m (cc/some? v1) (cc/assoc k1 v1) (cc/some? v2) (cc/assoc k2 v2)))
           ([m k1 v1 k2 v2 k3 v3] (cc/cond-> m (cc/some? v1) (cc/assoc k1 v1) (cc/some? v2) (cc/assoc k2 v2) (cc/some? v3) (cc/assoc k3 v3)))
           ([m k1 v1 k2 v2 k3 v3 & kvs]
            (cc/reduce (cc/fn [m [k1 v1]] (this/assoc-some m k1 v1))
                       (this/assoc-some m k1 v1 k2 v2 k3 v3)
                       (cc/partition 2 kvs)))))))


(defunroll unroll-assoc-some
  "Associates a key k, with a value v in a map m, if and only if v is not nil."
  {}
  assoc-some-spec)

(deftest unroll-assoc-some-test
  (is (= (-> #'unroll-assoc-some meta :arglists)
         '([m k1 v1] [m k1 v1 k2 v2] [m k1 v1 k2 v2 k3 v3] [m k1 v1 k2 v2 k3 v3 & kvs])))
  (doseq [i (range 1 11)
          provide-nil (map set (comb/subsets (range i)))
          :let [args (mapcat (fn [i] [i (when-not (provide-nil i) i)])
                             (range i))]]
    (is (= (apply assoc-some {} args)
           (apply unroll-assoc-some {} args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/update-vals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_;;TODO
(defn update-vals
  "m f => {k (f v) ...}
  Given a map m and a function f of 1-argument, returns a new map where the keys of m
  are mapped to result of applying f to the corresponding values of m."
  {:added "1.11"}
  [m f]
  (with-meta
    (persistent!
     (reduce-kv (fn [acc k v] (assoc! acc k (f v)))
                (if (instance? clojure.lang.IEditableCollection m)
                  (transient m)
                  (transient {}))
                m))
    (meta m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/update-keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_;;TODO
(defn update-keys
  "m f => {(f k) v ...}
  Given a map m and a function f of 1-argument, returns a new map whose
  keys are the result of applying f to the keys of m, mapped to the
  corresponding values of m.
  f must return a unique key for each key of m, else the behavior is undefined."
  {:added "1.11"}
  [m f]
  (let [ret (persistent!
             (reduce-kv (fn [acc k v] (assoc! acc (f k) v))
                        (transient {})
                        m))]
    (with-meta ret (meta m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/invoke (https://clojure.atlassian.net/browse/CLJ-2342)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
(defn invoke
  "Invokes fn f using the provided args."
  ([f] (f))
  ([f x] (f x))
  ([f x y] (f x y))
  ([f x y z] (f x y z))
  ([f x y z & args] (apply f x y z args)))

(defn unroll-invoke-spec*
  "Generate a spec for an unrolling of clojure.core/invoke.
  
   :size  Number of fixed arities to generate. Default: 4"
  ([] (unroll-invoke-spec* {}))
  ([{:keys [size] :or {size 4}}]
   {:argvs (uniformly-flowing-argvs
             {:arities (range size)
              :leading-names ['f]
              :fixed-names (single-char-syms-from \x)
              :rest-name 'args})
    :unroll-arity (fn [{[f & fixed-args] :fixed-args :keys [rest-arg]}]
                    (maybe-apply f fixed-args rest-arg))}))

(def unroll-invoke-spec (unroll-invoke-spec*))

(deftest unroll-invoke-spec-test
  (is (= (prettify-unroll (unroll-arities unroll-invoke-spec))
         '(([f] (f))
           ([f x] (f x))
           ([f x y] (f x y))
           ([f x y z] (f x y z))
           ([f x y z & args] (cc/apply f x y z args))))))

(defunroll unroll-invoke
  "Invokes fn f using the provided args."
  {}
  unroll-invoke-spec)

(deftest unroll-invoke-test
  (dotimes [i 10]
    (is (= (clojure.core/apply unroll-invoke + (range i))
           (apply + (range i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cljs.core/apply-to-simple
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
(defn- apply-to-simple
  "Internal. DO NOT USE!
  Assumes args was already called with seq beforehand!"
  ([f ^seq args]
   (if (nil? args)
     (if (.-cljs$core$IFn$_invoke$arity$0 f)
       (.cljs$core$IFn$_invoke$arity$0 f)
       (.call f f))
     (apply-to-simple f (-first args) (next* args))))
  ([f a0 ^seq args]
   (if (nil? args)
     (if (.-cljs$core$IFn$_invoke$arity$1 f)
       (.cljs$core$IFn$_invoke$arity$1 f a0)
       (.call f f a0))
     (apply-to-simple f a0 (-first args) (next* args))))
  ([f a0 a1 ^seq args]
   (if (nil? args)
     (if (.-cljs$core$IFn$_invoke$arity$2 f)
       (.cljs$core$IFn$_invoke$arity$2 f a0 a1)
       (.call f f a0 a1))
     (apply-to-simple f a0 a1 (-first args) (next* args))))
  ([f a0 a1 a2 ^seq args]
   (if (nil? args)
     (if (.-cljs$core$IFn$_invoke$arity$3 f)
       (.cljs$core$IFn$_invoke$arity$3 f a0 a1 a2)
       (.call f f a0 a1 a2))
     (apply-to-simple f a0 a1 a2 (-first args) (next* args))))
  ([f a0 a1 a2 a3 ^seq args]
   (if (nil? args)
     (if (.-cljs$core$IFn$_invoke$arity$4 f)
       (.cljs$core$IFn$_invoke$arity$4 f a0 a1 a2 a3)
       (.call f f a0 a1 a2 a3))
     (gen-apply-to-simple f 4 args))))

(defn unroll-apply-to-simple-spec*
  ([] (unroll-apply-to-simple-spec* {}))
  ([{:keys [size gen-apply-to-simple]
     :or {size 5
          gen-apply-to-simple 'cljs.core/gen-apply-to-simple}}]
   {:argvs (uniformly-flowing-argvs
             {:arities (range size)
              :leading-names ['f]
              :fixed-names (map #(symbol (str 'a %)) (range))
              :trailing-names ['^seq args]
              :rest-arity :skip})
    :unroll-arity (fn [{[f & args] :fixed-args apply-to-simple :this :keys [argv argvs]}]
                    (assert apply-to-simple)
                    (let [[as args] ((juxt butlast last) args)
                          _ (assert args)
                          final-arity? (= argv (apply max-key (comp count argv->fixed-args) argvs))
                          n (count as)
                          invoke-arity (str 'cljs$core$IFn$_invoke$arity$ n)
                          invoke-prop (symbol (str ".-" invoke-arity))
                          invoke-inv (symbol (str "." invoke-arity))]
                      `(if (nil? ~args)
                         (if (~invoke-prop ~f)
                           (~invoke-inv ~f ~@as)
                           (.call ~f ~f ~@as))
                         ~(if final-arity?
                            `(~gen-apply-to-simple ~f ~n ~args)
                            `(~apply-to-simple ~f ~@as (cljs.core/-first ~args) (cljs.core/next* ~args))))))}))

(def unroll-apply-to-simple-spec (unroll-apply-to-simple-spec*))

(deftest unroll-apply-to-simple-spec-test
  (is (= (prettify-unroll (unroll-arities (assoc unroll-apply-to-simple-spec :this 'this/unroll-to-apply)))
         '(([f args] (if (cc/nil? args)
                       (if (.-cljs$core$IFn$_invoke$arity$0 f) (.cljs$core$IFn$_invoke$arity$0 f) (.call f f))
                       (this/unroll-to-apply f (cljs.core/-first args) (cljs.core/next* args))))
           ([f a0 args] (if (cc/nil? args)
                          (if (.-cljs$core$IFn$_invoke$arity$1 f) (.cljs$core$IFn$_invoke$arity$1 f a0) (.call f f a0))
                          (this/unroll-to-apply f a0 (cljs.core/-first args) (cljs.core/next* args))))
           ([f a0 a1 args] (if (cc/nil? args)
                             (if (.-cljs$core$IFn$_invoke$arity$2 f) (.cljs$core$IFn$_invoke$arity$2 f a0 a1) (.call f f a0 a1))
                             (this/unroll-to-apply f a0 a1 (cljs.core/-first args) (cljs.core/next* args))))
           ([f a0 a1 a2 args] (if (cc/nil? args)
                                (if (.-cljs$core$IFn$_invoke$arity$3 f) (.cljs$core$IFn$_invoke$arity$3 f a0 a1 a2) (.call f f a0 a1 a2))
                                (this/unroll-to-apply f a0 a1 a2 (cljs.core/-first args) (cljs.core/next* args))))
           ([f a0 a1 a2 a3 args] (if (cc/nil? args)
                                   (if (.-cljs$core$IFn$_invoke$arity$4 f) (.cljs$core$IFn$_invoke$arity$4 f a0 a1 a2 a3) (.call f f a0 a1 a2 a3))
                                   (cljs.core/gen-apply-to-simple f 4 args))))))
  (is (= (->> (prettify-unroll (unroll-arities (assoc unroll-apply-to-simple-spec :this 'this/unroll-to-apply)))
              (map (comp meta peek first)))
         (repeat 5 {:tag 'seq}))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cljs.core/apply
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;TODO
#_
(defn apply
  "Applies fn f to the argument list formed by prepending intervening arguments to args."
  ([f args]
   (if (.-cljs$lang$applyTo f)
     (let [fixed-arity (.-cljs$lang$maxFixedArity f)
           bc (bounded-count (inc fixed-arity) args)]
       (if (<= bc fixed-arity)
         (apply-to f bc args)
         (.cljs$lang$applyTo f args)))
     (apply-to-simple f (seq args))))
  ([f x args]
   (if (.-cljs$lang$applyTo f)
     (let [arglist (list* x args)
           fixed-arity (.-cljs$lang$maxFixedArity f)
           bc (inc (bounded-count fixed-arity args))]
       (if (<= bc fixed-arity)
         (apply-to f bc arglist)
         (.cljs$lang$applyTo f arglist)))
     (apply-to-simple f x (seq args))))
  ([f x y args]
   (if (.-cljs$lang$applyTo f)
     (let [arglist (list* x y args)
           fixed-arity (.-cljs$lang$maxFixedArity f)
           bc (+ 2 (bounded-count (dec fixed-arity) args))]
       (if (<= bc fixed-arity)
         (apply-to f bc arglist)
         (.cljs$lang$applyTo f arglist)))
     (apply-to-simple f x y (seq args))))
  ([f x y z args]
   (if (.-cljs$lang$applyTo f)
     (let [arglist (list* x y z args)
           fixed-arity (.-cljs$lang$maxFixedArity f)
           bc (+ 3 (bounded-count (- fixed-arity 2) args))]
       (if (<= bc fixed-arity)
         (apply-to f bc arglist)
         (.cljs$lang$applyTo f arglist)))
     (apply-to-simple f x y z (seq args))))
  ([f a b c d & args]
   (if (.-cljs$lang$applyTo f)
     (let [spread-args (spread args)
           arglist (cons a (cons b (cons c (cons d spread-args))))
           fixed-arity (.-cljs$lang$maxFixedArity f)
           bc (+ 4 (bounded-count (- fixed-arity 3) spread-args))]
       (if (<= bc fixed-arity)
         (apply-to f bc arglist)
         (.cljs$lang$applyTo f arglist)))
     (apply-to-simple f a b c d (spread args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cljs.core/vary-meta
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO
#_
(defn vary-meta
 "Returns an object of the same type and value as obj, with
  (apply f (meta obj) args) as its metadata."
  ([obj f]
   (with-meta obj (f (meta obj))))
  ([obj f a]
   (with-meta obj (f (meta obj) a)))
  ([obj f a b]
   (with-meta obj (f (meta obj) a b)))
  ([obj f a b c]
   (with-meta obj (f (meta obj) a b c)))
  ([obj f a b c d]
   (with-meta obj (f (meta obj) a b c d)))
  ([obj f a b c d & args]
   (with-meta obj (apply f (meta obj) a b c d args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cljs.core/comp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO
#_
(defn comp
  "Takes a set of functions and returns a fn that is the composition
  of those fns.  The returned fn takes a variable number of args,
  applies the rightmost of fns to the args, the next
  fn (right-to-left) to the result, etc."
  ([] identity)
  ([f] f)
  ([f g]
     (fn
       ([] (f (g)))
       ([x] (f (g x)))
       ([x y] (f (g x y)))
       ([x y z] (f (g x y z)))
       ([x y z & args] (f (apply g x y z args)))))
  ([f g h]
     (fn
       ([] (f (g (h))))
       ([x] (f (g (h x))))
       ([x y] (f (g (h x y))))
       ([x y z] (f (g (h x y z))))
       ([x y z & args] (f (g (apply h x y z args))))))
  ([f1 f2 f3 & fs]
    (let [fs (reverse (list* f1 f2 f3 fs))]
      (fn [& args]
        (loop [ret (apply (first fs) args) fs (next fs)]
          (if fs
            (recur ((first fs) ret) (next fs))
            ret))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cljs.core/MultiFn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#_
(deftype MultiFn [name dispatch-fn default-dispatch-val hierarchy
                  method-table prefer-table method-cache cached-hierarchy]
  IFn
  (-invoke [mf]
    (let [dispatch-val (dispatch-fn)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (target-fn)))
  (-invoke [mf a]
    (let [dispatch-val (dispatch-fn a)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (target-fn a)))
  (-invoke [mf a b]
    (let [dispatch-val (dispatch-fn a b)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (target-fn a b)))
  (-invoke [mf a b c]
    (let [dispatch-val (dispatch-fn a b c)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (target-fn a b c)))
  (-invoke [mf a b c d]
    (let [dispatch-val (dispatch-fn a b c d)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (target-fn a b c d)))
  (-invoke [mf a b c d e]
    (let [dispatch-val (dispatch-fn a b c d e)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (target-fn a b c d e)))
  (-invoke [mf a b c d e f]
    (let [dispatch-val (dispatch-fn a b c d e f)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (target-fn a b c d e f)))
  (-invoke [mf a b c d e f g]
    (let [dispatch-val (dispatch-fn a b c d e f g)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (target-fn a b c d e f g)))
  (-invoke [mf a b c d e f g h]
    (let [dispatch-val (dispatch-fn a b c d e f g h)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (target-fn a b c d e f g h)))
  (-invoke [mf a b c d e f g h i]
    (let [dispatch-val (dispatch-fn a b c d e f g h i)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (target-fn a b c d e f g h i)))
  (-invoke [mf a b c d e f g h i j]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (target-fn a b c d e f g h i j)))
  (-invoke [mf a b c d e f g h i j k]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j k)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (target-fn a b c d e f g h i j k)))
  (-invoke [mf a b c d e f g h i j k l]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j k l)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (target-fn a b c d e f g h i j k l)))
  (-invoke [mf a b c d e f g h i j k l m]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j k l m)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (target-fn a b c d e f g h i j k l m)))
  (-invoke [mf a b c d e f g h i j k l m n]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j k l m n)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (target-fn a b c d e f g h i j k l m n)))
  (-invoke [mf a b c d e f g h i j k l m n o]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j k l m n o)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (target-fn a b c d e f g h i j k l m n o)))
  (-invoke [mf a b c d e f g h i j k l m n o p]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j k l m n o p)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (target-fn a b c d e f g h i j k l m n o p)))
  (-invoke [mf a b c d e f g h i j k l m n o p q]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j k l m n o p q)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (target-fn a b c d e f g h i j k l m n o p q)))
  (-invoke [mf a b c d e f g h i j k l m n o p q r]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j k l m n o p q r)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (target-fn a b c d e f g h i j k l m n o p q r)))
  (-invoke [mf a b c d e f g h i j k l m n o p q r s]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j k l m n o p q r s)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (target-fn a b c d e f g h i j k l m n o p q r s)))
  (-invoke [mf a b c d e f g h i j k l m n o p q r s t]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j k l m n o p q r s t)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (target-fn a b c d e f g h i j k l m n o p q r s t)))
  (-invoke [mf a b c d e f g h i j k l m n o p q r s t rest]
    (let [dispatch-val (apply dispatch-fn a b c d e f g h i j k l m n o p q r s t rest)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (apply target-fn a b c d e f g h i j k l m n o p q r s t rest)))

  IMultiFn
  (-reset [mf]
    (swap! method-table (fn [mf] {}))
    (swap! method-cache (fn [mf] {}))
    (swap! prefer-table (fn [mf] {}))
    (swap! cached-hierarchy (fn [mf] nil))
    mf)

  (-add-method [mf dispatch-val method]
    (swap! method-table assoc dispatch-val method)
    (reset-cache method-cache method-table cached-hierarchy hierarchy)
    mf)

  (-remove-method [mf dispatch-val]
    (swap! method-table dissoc dispatch-val)
    (reset-cache method-cache method-table cached-hierarchy hierarchy)
    mf)

  (-get-method [mf dispatch-val]
    (when-not (= @cached-hierarchy @hierarchy)
      (reset-cache method-cache method-table cached-hierarchy hierarchy))
    (if-let [target-fn (@method-cache dispatch-val)]
      target-fn
      (find-and-cache-best-method name dispatch-val hierarchy method-table
        prefer-table method-cache cached-hierarchy default-dispatch-val)))

  (-prefer-method [mf dispatch-val-x dispatch-val-y]
    (when (prefers* dispatch-val-y dispatch-val-x  prefer-table)
      (throw (js/Error. (str "Preference conflict in multimethod '" name "': " dispatch-val-y
                   " is already preferred to " dispatch-val-x))))
    (swap! prefer-table
           (fn [old]
             (assoc old dispatch-val-x
                    (conj (get old dispatch-val-x #{})
                          dispatch-val-y))))
    (reset-cache method-cache method-table cached-hierarchy hierarchy))

  (-methods [mf] @method-table)
  (-prefers [mf] @prefer-table)
  (-default-dispatch-val [mf] default-dispatch-val)
  (-dispatch-fn [mf] dispatch-fn)

  INamed
  (-name [this] (-name name))
  (-namespace [this] (-namespace name))

  IHash
  (-hash [this] (goog/getUid this)))

(defn unroll-cljs-invoke-impl [{:keys [this-name unroll-arity]
                                :or {this-name 'this}}]
  {:argvs (uniformly-flowing-argvs
            {:arities (range 22)
             :leading-names ['this-name]
             :fixed-names (concat (take 20 (single-char-syms-from \a))
                                  ['rest])
             :rest-arity :skip})
   :unroll-arity (fn [{[this & args :as fixed-args] :fixed-args :as m}]
                   (let [[fixed-args rest-arg] (if (= 21 (count args))
                                                 ((juxt pop peek) (vec args))
                                                 [(vec args) nil])]
                     (list '-invoke fixed-args
                           (unroll-arity (assoc m :this this :fixed-args fixed-args :rest-arg rest-arg)))))})

(defn unroll-MultiFn-invoke-spec*
  ([] (unroll-MultiFn-invoke-spec* {}))
  ([{:keys [dispatch-fn name]
     :or {dispatch-fn 'dispatch-fn
          name 'name}}]
   (unroll-cljs-invoke-impl
     {:this-name 'mf
      :unroll-arity (fn [{:keys [fixed-args rest-arg this]}]
                      (assert this)
                      (let [[dispatch-val target-fn] (map gensym-pretty '[dispatch-val target-fn])]
                        `(let [~dispatch-val ~(maybe-apply dispatch-fn fixed-args rest-arg)
                               ~target-fn (cljs.core/-get-method ~this ~dispatch-val)]
                           (when-not ~target-fn
                             (cljs.core/throw-no-method-error ~name ~dispatch-val))
                           ~(maybe-apply target-fn fixed-args rest-arg))))})))

(def unroll-MultiFn-invoke-spec (unroll-MultiFn-invoke-spec*))

(deftest unroll-MultiFn-invoke-spec-test
  (is (= (last (prettify-unroll (unroll-arities unroll-MultiFn-invoke-spec)))
         ;;FIXME double argv
         '([this-name a b c d e f g h i j k l m n o p q r s t rest]
           (-invoke [a b c d e f g h i j k l m n o p q r s t]
                    (cc/let [dispatch-val (cc/apply dispatch-fn a b c d e f g h i j k l m n o p q r s t rest)
                             target-fn (cljs.core/-get-method this-name dispatch-val)]
                      (cc/when-not target-fn (cljs.core/throw-no-method-error name dispatch-val))
                      (cc/apply target-fn a b c d e f g h i j k l m n o p q r s t rest)))))))
