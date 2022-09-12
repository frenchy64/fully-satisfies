(ns io.github.frenchy64.fully-satisfies.unrolling-macro-test
  (:require [clojure.core :as cc]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.unrolling-macro
             :refer [defunrolled unrolled-fn-tail gensym-pretty prettify-unrolled
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

(def unrolled-vector-spec
  {:argvs (uniformly-flowing-argvs
            {:arities (range 8)
             :fixed-names (single-char-syms-from \a)
             :rest-name 'args})
   :unrolled-arity (fn [_ fixed-args rest-arg]
                     (if rest-arg
                       `(clojure.lang.LazilyPersistentVector/create
                          ~(reduce (fn [acc x] `(cons ~x ~acc)) rest-arg (rseq fixed-args)))
                       fixed-args))})

(deftest unrolled-vector-spec-test
  (is (= (prettify-unrolled (unrolled-fn-tail unrolled-vector-spec))
         '(([] [])
           ([a] [a])
           ([a b] [a b])
           ([a b c] [a b c])
           ([a b c d] [a b c d])
           ([a b c d e] [a b c d e])
           ([a b c d e f] [a b c d e f])
           ([a b c d e f & args] (clojure.lang.LazilyPersistentVector/create (cc/cons a (cc/cons b (cc/cons c (cc/cons d (cc/cons e (cc/cons f args))))))))))))

(defunrolled unrolled-vector
  "Creates a new vector containing the args."
  {:added "1.0"
   :static true}
  unrolled-vector-spec)

(deftest unrolled-vector-test
  (is (= (-> #'unrolled-vector meta :arglists)
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

(def unrolled-list*-spec
  {:argvs (let [rest-arity 5]
            (assert (pos? rest-arity))
            (-> (mapv (fn [i]
                        (-> (into [] (take (dec i))
                                  (single-char-syms-from \a))
                            (conj 'args)))
                      (range 1 rest-arity))
                (conj (-> (into [] (take (dec rest-arity))
                                (single-char-syms-from \a))
                          (conj '& 'more)))))
   :unrolled-arity (fn [_ fixed-args rest-arg]
                     (if (and (= 1 (count fixed-args)) (not rest-arg))
                       `(seq ~(first fixed-args))
                       (reduce (fn [acc x] `(cons ~x ~acc))
                               (if rest-arg `(#'clojure.core/spread ~rest-arg) (peek fixed-args))
                               (rseq (pop fixed-args)))))})

(deftest unrolled-list*-spec-test
  (is (= (prettify-unrolled (unrolled-fn-tail unrolled-list*-spec))
         '(([args] (cc/seq args))
           ([a args] (cc/cons a args))
           ([a b args] (cc/cons a (cc/cons b args)))
           ([a b c args] (cc/cons a (cc/cons b (cc/cons c args))))
           ([a b c d & more] (cc/cons a (cc/cons b (cc/cons c ((var cc/spread) more)))))))))

(defunrolled unrolled-list*
  "Creates a new seq containing the items prepended to the rest, the
  last of which will be treated as a sequence."
  {:added "1.0"
   :static true}
  unrolled-list*-spec)

(deftest unrolled-list*-test
  (is (= (-> #'unrolled-list* meta :arglists)
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

(def unrolled-apply-spec
  {:argvs (let [rest-arity 6
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
   :unrolled-arity (fn [_ [f & fixed-args] rest-arg]
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
                                     `(list* ~@fixed-args ~last-fixed)))))))})

(deftest unrolled-apply-spec-test
  (is (= (prettify-unrolled (unrolled-fn-tail unrolled-apply-spec))
         '(([f args] (. f (applyTo (cc/seq args))))
           ([f x args] (. f (applyTo (cc/list* x args))))
           ([f x y args] (. f (applyTo (cc/list* x y args))))
           ([f x y z args] (. f (applyTo (cc/list* x y z args))))
           ([f a b c d & args] (. f (applyTo (cc/cons a (cc/cons b (cc/cons c (cc/cons d ((var cc/spread) args))))))))))))

(defunrolled unrolled-apply
  "Applies fn f to the argument list formed by prepending intervening arguments to args."
  {:added "1.0"
   :static true}
  unrolled-apply-spec)

(deftest unrolled-apply-test
  (is (= (-> #'unrolled-apply meta :arglists)
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

(def unrolled-comp-spec
  {:argvs (uniformly-flowing-argvs
            {:arities (range 4)
             :fixed-names (single-char-syms-from \f)
             :rest-name 'fs})
   :unrolled-arity (fn [this fixed-fs rest-fs]
                     (assert this)
                     (if rest-fs
                       `(reduce ~this ~(maybe-list* fixed-fs rest-fs))
                       (case (count fixed-fs)
                         0 `identity
                         1 (first fixed-fs)
                         `(fn ~@(unrolled-fn-tail
                                  {:argvs (uniformly-flowing-argvs
                                            {:arities (range 5)
                                             :fixed-names (single-char-syms-from \x)
                                             :rest-name 'args})
                                   :unrolled-arity (fn [_ fixed-args rest-args]
                                                     (reduce (fn [acc outer-f]
                                                               (list outer-f acc))
                                                             (maybe-apply (peek fixed-fs) fixed-args rest-args)
                                                             (pop fixed-fs)))})))))})
(deftest unrolled-comp-spec-test
  (is (= (prettify-unrolled (unrolled-fn-tail (assoc unrolled-comp-spec :this 'unrolled-comp)))
         '(([] cc/identity)
           ([f] f)
           ([f g] (cc/fn 
                    ([] (f (g)))
                    ([x] (f (g x)))
                    ([x y] (f (g x y)))
                    ([x y z] (f (g x y z)))
                    ([x y z & args] (f (cc/apply g x y z args)))))
           ([f g & fs] (cc/reduce unrolled-comp (cc/list* f g fs)))))))

(defunrolled unrolled-comp
  "Takes a set of functions and returns a fn that is the composition
  of those fns.  The returned fn takes a variable number of args,
  applies the rightmost of fns to the args, the next
  fn (right-to-left) to the result, etc."
  {:added "1.0"
   :static true}
  unrolled-comp-spec)

(deftest unrolled-comp-test
  (is (= (-> #'unrolled-comp meta :arglists)
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

(def unrolled-juxt-spec
  {:argvs (uniformly-flowing-argvs
            {:arities (range 1 5)
             :fixed-names (single-char-syms-from \f)
             :rest-name 'fs})
   :unrolled-arity (fn [_ fixed-fs rest-fs]
                     (let [fs (gensym-pretty 'fs)
                           body `(fn ~@(unrolled-fn-tail
                                         {:argvs (uniformly-flowing-argvs
                                                   {:arities (range 5)
                                                    :fixed-names (single-char-syms-from \x)
                                                    :rest-name 'args})
                                          :unrolled-arity (fn [_ fixed-args rest-args]
                                                            (if rest-fs
                                                              (let [v (gensym-pretty 'acc)
                                                                    f (gensym-pretty 'f)]
                                                                `(reduce (fn [~v ~f] (conj ~v ~(maybe-apply f fixed-args rest-args))) [] ~fs))
                                                              (mapv #(maybe-apply % fixed-args rest-args) fixed-fs)))}))]
                       (if rest-fs
                         `(let [~fs ~(maybe-list* fixed-fs rest-fs)] ~body)
                         body)))})

(deftest unrolled-juxt-spec-test
  (is (= (prettify-unrolled (unrolled-fn-tail unrolled-juxt-spec))
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

(defunrolled unrolled-juxt 
  "Takes a set of functions and returns a fn that is the juxtaposition
  of those fns.  The returned fn takes a variable number of args, and
  returns a vector containing the result of applying each fn to the
  args (left-to-right).
  ((juxt a b c) x) => [(a x) (b x) (c x)]"
  {:added "1.1"
   :static true}
  unrolled-juxt-spec)

(deftest unrolled-juxt-test
  (is (= (-> #'unrolled-juxt meta :arglists)
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

(def unrolled-partial-spec
  {:argvs (uniformly-flowing-argvs
            {:arities (range 1 6)
             :fixed-names (cons 'f (map #(symbol (str "arg" %)) (next (range))))
             :rest-name 'more})
   :unrolled-arity (fn [_ [f & fixed-args] rest-args]
                     `(fn ~@(unrolled-fn-tail
                              {:argvs (uniformly-flowing-argvs
                                        {:arities (range (if rest-args 0 5))
                                         :fixed-names (single-char-syms-from \x)
                                         :rest-name 'args})
                               :unrolled-arity (fn [_ fixed-additional-args rest-additional-args]
                                                 (maybe-apply f
                                                              (concat fixed-args fixed-additional-args)
                                                              (maybe-concat rest-additional-args rest-args)))})))})

(deftest unrolled-partial-spec-test
  (is (= (prettify-unrolled (unrolled-fn-tail unrolled-partial-spec))
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

(defunrolled unrolled-partial
  "Takes a function f and fewer than the normal arguments to f, and
  returns a fn that takes a variable number of additional args. When
  called, the returned function calls f with args + additional args."
  {:added "1.0"
   :static true}
  unrolled-partial-spec)

(deftest unrolled-partial-test
  (is (= (-> #'unrolled-partial meta :arglists)
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

(def unrolled-naive-everyp-spec
  {:argvs (let [rest-arity 4]
            (assert (<= 2 rest-arity))
            (uniformly-flowing-argvs
              {:arities (range 0 (inc rest-arity))
               :fixed-names (map #(symbol (str "p" %)) (next (range)))
               :rest-name 'ps}))
   :unrolled-arity (fn [_ fixed-preds rest-pred]
                     `(fn ~@(unrolled-fn-tail
                              {:argvs (uniformly-flowing-argvs
                                        {:arities (range 5)
                                         :fixed-names (single-char-syms-from \x)
                                         :rest-name 'args})
                               :unrolled-arity (fn [_ fixed-args rest-arg]
                                                 (let [tp (fn [p]
                                                            (cond-> (mapv #(list p %) fixed-args)
                                                              rest-arg (conj (maybe-every? p rest-arg))))]
                                                   (maybe-boolean
                                                     (maybe-and (-> []
                                                                    (into (mapcat tp fixed-preds))
                                                                    (cond->
                                                                      rest-pred (conj (let [p (gensym-pretty 'p)]
                                                                                        (maybe-every? `(fn [~p] ~(maybe-and (tp p))) rest-pred)))))))))})))})

(deftest unrolled-naive-everyp-spec-test
  (is (= (prettify-unrolled (unrolled-fn-tail unrolled-naive-everyp-spec))
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

(def unrolled-naive-somef-spec
  {:argvs (let [rest-arity 4]
            (assert (<= 2 rest-arity))
            (uniformly-flowing-argvs
              {:arities (range 0 (inc rest-arity))
               :fixed-names (map #(symbol (str "f" %)) (next (range)))
               :rest-name 'fs}))
   :unrolled-arity (fn [_ fixed-fs rest-f]
                     `(fn ~@(unrolled-fn-tail
                              {:argvs (uniformly-flowing-argvs
                                        {:arities (range 5)
                                         :fixed-names (single-char-syms-from \x)
                                         :rest-name 'args})
                               :unrolled-arity (fn [_ fixed-args rest-arg]
                                                 (let [f-tests (fn [f] (mapv #(list f %) fixed-args))]
                                                   `(or ~@(-> []
                                                              (into (mapcat #(cond-> (f-tests %)
                                                                               rest-arg (conj `(cc/some ~% ~rest-arg)))
                                                                            fixed-fs))
                                                              (conj (when rest-f
                                                                      (let [f (gensym-pretty 'f)]
                                                                        `(cc/some (fn [~f] (and ~@(f-tests f))) ~rest-arg))))))))})))})
