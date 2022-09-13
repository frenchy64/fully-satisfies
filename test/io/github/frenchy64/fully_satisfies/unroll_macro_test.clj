(ns io.github.frenchy64.fully-satisfies.unroll-macro-test
  (:require [clojure.core :as cc]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.unroll-macro
             :refer [defunroll unroll-arities gensym-pretty prettify-unroll
                     single-char-syms-from
                     uniformly-flowing-argvs]]))

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

(defn maybe-and [exprs]
  (let [exprs (remove true? exprs)]
    (case (count exprs)
      0 true
      1 (first exprs)
      `(and ~@exprs))))

(defn true-expression? [coll]
  (true? coll))

(defn single-arg-true-function? [coll]
  (boolean (when (seq? coll)
             (when (and (= 3 (count coll))
                        (= `fn (first coll))
                        (vector? (second coll))
                        (= 1 (count (second coll))))
               (true-expression? (last coll))))))

(deftest single-arg-true-function?-test
  (is (single-arg-true-function? `(fn [_] true)))
  (is (not (single-arg-true-function? `(fn [] true))))
  (is (not (single-arg-true-function? `(fn [_] false)))))

(defn maybe-every? [f coll]
  (if (single-arg-true-function? f)
    true
    `(every? ~f ~coll)))

(deftest maybe-every?-test
  (is (= true (maybe-every? `(fn [_] true) 'coll)))
  (is (= `(every? (fn [~'_ ~'_] true) ~'coll) (maybe-every? `(fn [~'_ ~'_] true) 'coll)))
  (is (= `(every? (fn [~'p] ~'p) ~'coll) (maybe-every? `(fn [~'p] ~'p) 'coll))))

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
    :unroll-arity (fn [_ fixed-args rest-arg]
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
         '([] [a] [a b] [a b c] [a b c d] [a b c d e] [a b c d e f] [a b c d e f & args]))))



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
    :unroll-arity (fn [_ fixed-args rest-arg]
                    (if (and (= 1 (count fixed-args)) (not rest-arg))
                      `(seq ~(first fixed-args))
                      (reduce (fn [acc x] `(cons ~x ~acc))
                              (if rest-arg `(#'clojure.core/spread ~rest-arg) (peek fixed-args))
                              (some-> fixed-args not-empty pop rseq))))}))

(def unroll-list*-spec (unroll-list*-spec*))

(deftest unroll-list*-spec-test
  (is (= (prettify-unroll (unroll-arities (unroll-list*-spec* {:size 0})))
         '([& more] ((var cc/spread) more))))
  (is (= (prettify-unroll (unroll-arities (unroll-list*-spec* {:size 1})))
         '(([args] (cc/seq args)) 
           ([a & more] ((var cc/spread) more)))))
  (is (= (prettify-unroll (unroll-arities (unroll-list*-spec* {:size 2})))
         '(([args] (cc/seq args)) 
           ([a args] (cc/cons a args)) 
           ([a b & more] (cc/cons a ((var cc/spread) more))))))
  (is (= (prettify-unroll (unroll-arities unroll-list*-spec))
         '(([args] (cc/seq args))
           ([a args] (cc/cons a args))
           ([a b args] (cc/cons a (cc/cons b args)))
           ([a b c args] (cc/cons a (cc/cons b (cc/cons c args))))
           ([a b c d & more] (cc/cons a (cc/cons b (cc/cons c ((var cc/spread) more)))))))))

(defunroll unroll-list*
  "Creates a new seq containing the items prepended to the rest, the
  last of which will be treated as a sequence."
  {:added "1.0"
   :static true}
  unroll-list*-spec)

(deftest unroll-list*-test
  (is (= (-> #'unroll-list* meta :arglists)
         (-> #'clojure.core/list* meta :arglists)
         '([args] [a args] [a b args] [a b c args] [a b c d & more]))))




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
  ([] (unroll-apply-spec* {}))
  ([{:keys [size] :or {size 4}}]
   {:argvs (let [rest-arity (+ 2 size)
                 f (with-meta 'f {:tag 'clojure.lang.IFn})
                 args 'args]
             (-> (mapv (fn [i]
                         (-> [f]
                             (into (take i) (single-char-syms-from \x))
                             (conj args)))
                       (range (- rest-arity 2)))
                 (conj (-> [f]
                           (into (take (- rest-arity 2))
                                 (single-char-syms-from \a))
                           (conj '& args)))))
    :unroll-arity (fn [_ [f & fixed-args] rest-arg]
                    (let [[fixed-args last-fixed] (if rest-arg
                                                    [fixed-args nil]
                                                    [(butlast fixed-args) (last fixed-args)])]
                      `(. ~f (~'applyTo
                               ~(if rest-arg
                                  (reduce (fn [acc x] `(cons ~x ~acc))
                                          `(#'clojure.core/spread ~rest-arg)
                                          (reverse fixed-args))
                                  (if (empty? fixed-args)
                                    `(seq ~last-fixed)
                                    `(list* ~@fixed-args ~last-fixed)))))))}))

(def unroll-apply-spec (unroll-apply-spec*))

(deftest unroll-apply-spec-test
  (is (= (prettify-unroll (unroll-arities (unroll-apply-spec* {:size 0})))
         '([f & args] (. f (applyTo ((var cc/spread) args))))))
  (is (= (prettify-unroll (unroll-arities (unroll-apply-spec* {:size 1})))
         '(([f args] (. f (applyTo (cc/seq args))))
           ([f a & args] (. f (applyTo (cc/cons a ((var cc/spread) args))))))))
  (is (= (prettify-unroll (unroll-arities (unroll-apply-spec* {:size 2})))
         '(([f args] (. f (applyTo (cc/seq args))))
           ([f x args] (. f (applyTo (cc/list* x args))))
           ([f a b & args] (. f (applyTo (cc/cons a (cc/cons b ((var cc/spread) args)))))))))
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
         '([f args] [f x args] [f x y args] [f x y z args] [f a b c d & args]))))


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
  ([] (unroll-comp-spec* {}))
  ([{:keys [outer-size inner-size] :or {outer-size 3 inner-size 4}}]
   {:argvs (uniformly-flowing-argvs
             {:arities (range outer-size)
              :fixed-names (single-char-syms-from \f)
              :rest-name 'fs})
    :unroll-arity (fn [this fixed-fs rest-fs]
                    (assert this)
                    (if rest-fs
                      `(reduce ~this ~(maybe-list* fixed-fs rest-fs))
                      (case (count fixed-fs)
                        0 `identity
                        1 (first fixed-fs)
                        `(fn ~@(unroll-arities
                                 {:argvs (uniformly-flowing-argvs
                                           {:arities (range inner-size)
                                            :fixed-names (single-char-syms-from \x)
                                            :rest-name 'args})
                                  :unroll-arity (fn [_ fixed-args rest-args]
                                                  (reduce (fn [acc outer-f]
                                                            (list outer-f acc))
                                                          (maybe-apply (peek fixed-fs) fixed-args rest-args)
                                                          (pop fixed-fs)))})))))}))

(def unroll-comp-spec (unroll-comp-spec*))

(deftest unroll-comp-spec-test
  (is (= (prettify-unroll (unroll-arities (assoc (unroll-comp-spec* {:outer-size 0 :inner-size 0}) :this 'unroll-comp)))
         '([& fs] (cc/reduce unroll-comp fs))))
  (is (= (prettify-unroll (unroll-arities (assoc (unroll-comp-spec* {:outer-size 1 :inner-size 0}) :this 'unroll-comp)))
         '(([] cc/identity)
           ([& fs] (cc/reduce unroll-comp fs)))))
  (is (= (prettify-unroll (unroll-arities (assoc (unroll-comp-spec* {:outer-size 2 :inner-size 0}) :this 'unroll-comp)))
         '(([] cc/identity) 
           ([f] f)
           ([f & fs] (cc/reduce unroll-comp (cc/list* f fs))))))
  (is (= (prettify-unroll (unroll-arities (assoc (unroll-comp-spec* {:outer-size 3 :inner-size 0}) :this 'unroll-comp)))
         '(([] cc/identity)
           ([f] f)
           ([f g] (cc/fn [& args] (f (cc/apply g args))))
           ([f g & fs] (cc/reduce unroll-comp (cc/list* f g fs))))))
  (is (= (prettify-unroll (unroll-arities (assoc (unroll-comp-spec* {:outer-size 3 :inner-size 1}) :this 'unroll-comp)))
         '(([] cc/identity)
           ([f] f)
           ([f g] (cc/fn
                    ([] (f (g)))
                    ([& args] (f (cc/apply g args)))))
           ([f g & fs] (cc/reduce unroll-comp (cc/list* f g fs))))))
  (is (= (prettify-unroll (unroll-arities (assoc (unroll-comp-spec* {:outer-size 3 :inner-size 2}) :this 'unroll-comp)))
         '(([] cc/identity)
           ([f] f)
           ([f g] (cc/fn
                    ([] (f (g)))
                    ([x] (f (g x)))
                    ([x & args] (f (cc/apply g x args)))))
           ([f g & fs] (cc/reduce unroll-comp (cc/list* f g fs))))))
  (is (= (prettify-unroll (unroll-arities (assoc (unroll-comp-spec* {:outer-size 4 :inner-size 0}) :this 'unroll-comp)))
         '(([] cc/identity)
           ([f] f)
           ([f g] (cc/fn [& args] (f (cc/apply g args))))
           ([f g h] (cc/fn [& args] (g (f (cc/apply h args)))))
           ([f g h & fs] (cc/reduce unroll-comp (cc/list* f g h fs))))))
  (is (= (prettify-unroll (unroll-arities (assoc unroll-comp-spec :this 'unroll-comp)))
         '(([] cc/identity)
           ([f] f)
           ([f g] (cc/fn 
                    ([] (f (g)))
                    ([x] (f (g x)))
                    ([x y] (f (g x y)))
                    ([x y z] (f (g x y z)))
                    ([x y z & args] (f (cc/apply g x y z args)))))
           ([f g & fs] (cc/reduce unroll-comp (cc/list* f g fs)))))))

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
         '([] [f] [f g] [f g & fs]))))






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
             {:arities (range 1 4) ;;hmm we can't use the same trick as (range 0).
              :fixed-names (single-char-syms-from \f)
              :rest-name 'fs})
    :unroll-arity (fn [_ fixed-fs rest-fs]
                    (let [fs (gensym-pretty 'fs)
                          body `(fn ~@(unroll-arities
                                        {:argvs (uniformly-flowing-argvs
                                                  {:arities (range 4)
                                                   :fixed-names (single-char-syms-from \x)
                                                   :rest-name 'args})
                                         :unroll-arity (fn [_ fixed-args rest-args]
                                                         (if rest-fs
                                                           (let [v (gensym-pretty 'acc)
                                                                 f (gensym-pretty 'f)]
                                                             `(reduce (fn [~v ~f] (conj ~v ~(maybe-apply f fixed-args rest-args))) [] ~fs))
                                                           (mapv #(maybe-apply % fixed-args rest-args) fixed-fs)))}))]
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
         '([f] [f g] [f g h] [f g h & fs]))))







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
            {:arities (range 1 5)
             :fixed-names (cons 'f (map #(symbol (str "arg" %)) (next (range))))
             :rest-name 'more})
   :unroll-arity (fn [_ [f & fixed-args] rest-args]
                   `(fn ~@(unroll-arities
                            {:argvs (uniformly-flowing-argvs
                                      {:arities (range (if rest-args 0 4))
                                       :fixed-names (single-char-syms-from \x)
                                       :rest-name 'args})
                             :unroll-arity (fn [_ fixed-additional-args rest-additional-args]
                                             (maybe-apply f
                                                          (concat fixed-args fixed-additional-args)
                                                          (maybe-concat rest-additional-args rest-args)))})))})

(deftest unroll-partial-spec-test
  (is (= (prettify-unroll (unroll-arities unroll-partial-spec))
         '(([f]
            (cc/fn
              ([] (f))
              ([x] (f x))
              ([x y] (f x y))
              ([x y z] (f x y z))
              ([x y z & args] (cc/apply f x y z args))))
           ([f arg1]
            (cc/fn
              ([] (f arg1))
              ([x] (f arg1 x))
              ([x y] (f arg1 x y))
              ([x y z] (f arg1 x y z)) ([x y z & args] (cc/apply f arg1 x y z args))))
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
            (cc/fn [& args] (cc/apply f arg1 arg2 arg3 (cc/concat args more))))))))

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
         '([f] [f arg1] [f arg1 arg2] [f arg1 arg2 arg3] [f arg1 arg2 arg3 & more]))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Naive everyp
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

;;TODO make `:smaller-arities?` threshold-based config (only kicks in after x-sized arities)
(defn unroll-naive-everyp-spec*
  ([] (unroll-naive-everyp-spec* {}))
  ([{:keys [outer-size inner-size smaller-arities?]
     :or {outer-size 4
          inner-size 4}}]
   (assert (nat-int? outer-size))
   (assert (nat-int? inner-size))
   {:argvs (uniformly-flowing-argvs
             {:arities (range outer-size)
              :fixed-names (map #(symbol (str "p" %)) (next (range)))
              :rest-name 'ps})
    :unroll-arity (fn [_ fixed-preds rest-pred]
                    `(fn ~@(unroll-arities
                             {:argvs (uniformly-flowing-argvs
                                       {:arities (range inner-size)
                                        :fixed-names (single-char-syms-from \x)
                                        :rest-name 'args})
                              :unroll-arity (fn [_ fixed-args rest-arg]
                                              (let [tp (when (and smaller-arities?
                                                                  (< 1 (cond-> (count fixed-preds) rest-pred inc))
                                                                  (< 1 (cond-> (count fixed-args) rest-arg inc)))
                                                         (gensym-pretty 'tp))
                                                    tp-gen (fn [p]
                                                             (cond-> (mapv #(list p %) fixed-args)
                                                               rest-arg (conj (maybe-every? p rest-arg))))
                                                    tp-call (fn [p] (if tp [`(~tp ~p)] (tp-gen p)))
                                                    p (gensym-pretty 'p)
                                                    body (maybe-boolean
                                                           (maybe-and (cond-> (into [] (mapcat tp-call) fixed-preds)
                                                                        rest-pred (conj (maybe-every? (or tp `(fn [~p] ~(maybe-and (tp-call p)))) rest-pred)))))]
                                                (if tp
                                                  `(let [~tp (fn [~p] ~(maybe-and (tp-gen p)))] ~body)
                                                  body)))})))}))

(def unroll-naive-everyp-spec (unroll-naive-everyp-spec*))

(deftest unroll-naive-everyp-spec-test
  (is (= (prettify-unroll (unroll-arities (unroll-naive-everyp-spec*
                                                {:outer-size 0
                                                 :inner-size 0})))
         '([& ps] (cc/fn [& args] (cc/every? (cc/fn [p] (cc/every? p args)) ps)))))
  (is (= (prettify-unroll (unroll-arities (unroll-naive-everyp-spec*
                                                {:outer-size 1
                                                 :inner-size 0})))

         '(([] (cc/fn [& args] true))
           ([& ps] (cc/fn [& args] (cc/every? (cc/fn [p] (cc/every? p args)) ps))))))
  (is (= (prettify-unroll (unroll-arities (unroll-naive-everyp-spec*
                                                {:outer-size 1
                                                 :inner-size 1})))


         '(([] (cc/fn
                 ([] true)
                 ([& args] true)))
           ([& ps] (cc/fn
                     ([] true)
                     ([& args] (cc/every? (cc/fn [p] (cc/every? p args)) ps)))))))
  (is (= (prettify-unroll (unroll-arities (unroll-naive-everyp-spec*
                                                {:outer-size 3
                                                 :inner-size 4
                                                 :smaller-arities? true})))

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
                      ([x y] (cc/let [tp (cc/fn [p] (cc/and (p x) (p y)))]
                               (cc/boolean (cc/and (tp p1)
                                                   (tp p2)))))
                      ([x y z] (cc/let [tp (cc/fn [p] (cc/and (p x) (p y) (p z)))]
                                 (cc/boolean (cc/and (tp p1)
                                                     (tp p2)))))
                      ([x y z & args] (cc/let [tp (cc/fn [p] (cc/and (p x) (p y) (p z) (cc/every? p args)))]
                                        (cc/boolean (cc/and (tp p1)
                                                            (tp p2)))))))
           ([p1 p2 & ps] (cc/fn
                           ([] true)
                           ([x] (cc/boolean (cc/and (p1 x) (p2 x) (cc/every? (cc/fn [p] (p x)) ps))))
                           ([x y] (cc/let [tp (cc/fn [p] (cc/and (p x) (p y)))]
                                    (cc/boolean (cc/and (tp p1)
                                                        (tp p2)
                                                        (cc/every? tp ps)))))
                           ([x y z] (cc/let [tp (cc/fn [p] (cc/and (p x) (p y) (p z)))]
                                      (cc/boolean (cc/and (tp p1)
                                                          (tp p2)
                                                          (cc/every? tp ps)))))
                           ([x y z & args] (cc/let [tp (cc/fn [p] (cc/and (p x) (p y) (p z) (cc/every? p args)))]
                                             (cc/boolean (cc/and (tp p1)
                                                                 (tp p2)
                                                                 (cc/every? tp ps))))))))))
  ;; potentially useful implementation. maybe the non-rest arities should fully unroll.
  (is (= (prettify-unroll (unroll-arities (unroll-naive-everyp-spec*
                                              {:smaller-arities? true}))
                            {:unqualify-core true})
         '(([] (fn ([] true) ([x] true) ([x y] true) ([x y z] true) ([x y z & args] true)))
           ([p1] (fn ([] true)
                   ([x] (boolean (p1 x)))
                   ([x y] (boolean (and (p1 x) (p1 y))))
                   ([x y z] (boolean (and (p1 x) (p1 y) (p1 z))))
                   ([x y z & args] (boolean (and (p1 x) (p1 y) (p1 z) (every? p1 args))))))
           ([p1 p2] (fn ([] true)
                      ([x] (boolean (and (p1 x) (p2 x))))
                      ([x y] (let [tp (fn [p] (and (p x) (p y)))]
                               (boolean (and (tp p1) (tp p2)))))
                      ([x y z] (let [tp (fn [p] (and (p x) (p y) (p z)))]
                                 (boolean (and (tp p1) (tp p2)))))
                      ([x y z & args] (let [tp (fn [p] (and (p x) (p y) (p z) (every? p args)))]
                                        (boolean (and (tp p1) (tp p2)))))))
           ([p1 p2 p3] (fn ([] true)
                         ([x] (boolean (and (p1 x) (p2 x) (p3 x))))
                         ([x y] (let [tp (fn [p] (and (p x) (p y)))]
                                  (boolean (and (tp p1) (tp p2) (tp p3)))))
                         ([x y z] (let [tp (fn [p] (and (p x) (p y) (p z)))]
                                    (boolean (and (tp p1) (tp p2) (tp p3)))))
                         ([x y z & args] (let [tp (fn [p] (and (p x) (p y) (p z) (every? p args)))]
                                           (boolean (and (tp p1) (tp p2) (tp p3)))))))
           ([p1 p2 p3 & ps] (fn ([] true)
                              ([x] (boolean (and (p1 x) (p2 x) (p3 x) (every? (fn [p] (p x)) ps))))
                              ([x y] (let [tp (fn [p] (and (p x) (p y)))]
                                       (boolean (and (tp p1) (tp p2) (tp p3) (every? tp ps)))))
                              ([x y z] (let [tp (fn [p] (and (p x) (p y) (p z)))]
                                         (boolean (and (tp p1) (tp p2) (tp p3) (every? tp ps)))))
                              ([x y z & args] (let [tp (fn [p] (and (p x) (p y) (p z) (every? p args)))]
                                                (boolean (and (tp p1) (tp p2) (tp p3) (every? tp ps))))))))))
  (is (= (prettify-unroll (unroll-arities unroll-naive-everyp-spec))
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
       ([] (or nil))
       ([x] (or nil))
       ([x y] (or nil))
       ([x y z] (or nil))
       ([x y z & args] (or nil))))
  ([f1]
     (fn
       ([] (or nil))
       ([x] (or (f1 x) nil))
       ([x y] (or (f1 x) (f1 y)
                  nil))
       ([x y z] (or (f1 x) (f1 y) (f1 z)
                    nil))
       ([x y z & args] (or (f1 x) (f1 y) (f1 z) (some f1 args)
                           nil))))
  ([f1 f2]
     (fn
       ([] (or nil))
       ([x] (or (f1 x) (f2 x)
                nil))
       ([x y] (or (f1 x) (f1 y)
                  (f2 x) (f2 y)
                  nil))
       ([x y z] (or (f1 x) (f1 y) (f1 z)
                    (f2 x) (f2 y) (f2 z)
                    nil))
       ([x y z & args] (or (f1 x) (f1 y) (f1 z) (some f1 args)
                           (f2 x) (f2 y) (f2 z) (some f2 args)
                           nil))))
  ([f1 f2 f3]
     (fn
       ([] (or nil))
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
                           (f3 x) (f3 y) (f3 z) (some f3 args)
                           nil))))
  ([f1 f2 f3 & fs]
   (fn
     ([] (or nil))
     ([x] (or (f1 x) (f2 x) (f3 x)
              (some #(or (% x)) fs)))
     ([x y] (or (f1 x) (f1 y)
                (f2 x) (f2 y)
                (f3 x) (f3 y)
                (some #(or (% x)) fs)))
     ([x y z] (or (f1 x) (f1 y) (f1 z)
                  (f2 x) (f2 y) (f2 z)
                  (f3 x) (f3 y) (f3 z)
                  (some #(or (% x) (% y)) fs)))
     ([x y z & args] (or (f1 x) (f1 y) (f1 z) (some f1 args)
                         (f2 x) (f2 y) (f2 z) (some f2 args)
                         (f3 x) (f3 y) (f3 z) (some f3 args)
                         (some #(or (% x) (% y) (% z)) fs))))))

;; TODO unit test
(def unroll-naive-somef-spec
  {:argvs (let [rest-arity 4]
            (assert (<= 2 rest-arity))
            (uniformly-flowing-argvs
              {:arities (range 0 (inc rest-arity))
               :fixed-names (map #(symbol (str "f" %)) (next (range)))
               :rest-name 'fs}))
   :unroll-arity (fn [_ fixed-fs rest-f]
                   `(fn ~@(unroll-arities
                            {:argvs (uniformly-flowing-argvs
                                      {:arities (range 5)
                                       :fixed-names (single-char-syms-from \x)
                                       :rest-name 'args})
                             :unroll-arity (fn [_ fixed-args rest-arg]
                                             (let [f-tests (fn [f] (mapv #(list f %) fixed-args))]
                                               `(or ~@(-> []
                                                          (into (mapcat #(cond-> (f-tests %)
                                                                           rest-arg (conj `(cc/some ~% ~rest-arg)))
                                                                        fixed-fs))
                                                          (conj (when rest-f
                                                                  (let [f (gensym-pretty 'f)]
                                                                    `(cc/some (fn [~f] (and ~@(f-tests f))) ~rest-arg))))))))})))})

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
   :unroll-arity (fn [_ [f & xs] _]
                   `(fn ~@(unroll-arities
                            {:argvs (uniformly-flowing-argvs
                                      {:arities (range (if (= 1 (count xs)) 1 2)
                                                       4)
                                       :fixed-names (single-char-syms-from \a)
                                       :rest-name 'ds})
                             :unroll-arity (fn [_ as ds]
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
         '([f x] [f x y] [f x y z]))))


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
   :unroll-arity (fn [_ [f & xs] _]
                   (let [frame (gensym-pretty 'frame)]
                     `(let [~frame (clojure.lang.Var/cloneThreadBindingFrame)]
                        (fn ~@(unroll-arities
                                {:argvs (uniformly-flowing-argvs
                                          {:arities (range 4)
                                           :fixed-names (single-char-syms-from \x)
                                           :rest-name 'args})
                                 :unroll-arity (fn [_ xs args]
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

(deftest unroll-binding-conveyor-fn-test
  (is (= (-> #'unroll-binding-conveyor-fn meta :arglists)
         (-> #'clojure.core/binding-conveyor-fn meta :arglists)
         '([f]))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/swap!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO
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
            {:arities (range 2 5)
             :fixed-names (list* (with-meta 'atom {:tag 'clojure.lang.IAtom})
                                 'f
                                 (single-char-syms-from \x))
             :rest-name 'args})
   :unroll-arity (fn [_ [atm f & xs] args]
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

;;TODO argv :tag

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
              {:arities (range 2 5)
               :fixed-names (list* (with-meta 'atom {:tag 'clojure.lang.IAtom2})
                                   'f
                                   (single-char-syms-from \x))
               :rest-name 'args}))
   :unroll-arity (fn [_ [atm f & xs] args]
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

;;TODO
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/complement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/interleave
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/memoize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/mapv
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/merge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO
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


