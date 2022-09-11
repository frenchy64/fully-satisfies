(ns io.github.frenchy64.fully-satisfies.unrolling-macro-test
  (:require [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.unrolling-macro
             :refer [defunrolled unrolled-fn-tail gensym-pretty prettify-unrolled]]))

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
  {:arities (range 4)
   :fixed-names (single-char-syms-from \f)
   :rest-name 'fs
   :unrolled-arity (fn [this fixed-fs rest-fs]
                     (if rest-fs
                       `(#'clojure.core/reduce1 ~this ~(maybe-list* fixed-fs rest-fs))
                       (case (count fixed-fs)
                         0 `identity
                         1 (first fixed-fs)
                         `(fn ~@(unrolled-fn-tail
                                  {:arities (range 5)
                                   :fixed-names (single-char-syms-from \x)
                                   :rest-name 'args
                                   :unrolled-arity (fn [_ fixed-args rest-args]
                                                     (reduce (fn [acc outer-f]
                                                               (list outer-f acc))
                                                             (maybe-apply (peek fixed-fs) fixed-args rest-args)
                                                             (pop fixed-fs)))})))))})

(comment
  (unrolled-fn-tail unrolled-comp-spec)
  (prettify-unrolled (unrolled-fn-tail unrolled-comp-spec))
  )

(deftest unrolled-comp-spec-test
  (is (= (prettify-unrolled (unrolled-fn-tail unrolled-comp-spec))
         '(([] cc/identity)
           ([f] f)
           ([f g] (cc/fn 
                    ([] (f (g)))
                    ([x] (f (g x)))
                    ([x y] (f (g x y)))
                    ([x y z] (f (g x y z)))
                    ([x y z & args] (f (cc/apply g x y z args)))))
           ([f g & fs] ((var cc/reduce1) nil (cc/list* f g fs)))))))

(defunrolled unrolled-comp
  "Takes a set of functions and returns a fn that is the composition
  of those fns.  The returned fn takes a variable number of args,
  applies the rightmost of fns to the args, the next
  fn (right-to-left) to the result, etc."
  {:added "1.0"
   :static true}
  unrolled-comp-spec)

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
  {:arities (range 1 5)
   :fixed-names (single-char-syms-from \f)
   :rest-name 'fs
   :unrolled-arity (fn [_ fixed-fs rest-fs]
                     (let [fs (gensym-pretty "fs")
                           body `(fn ~@(unrolled-fn-tail
                                         {:arities (range 5)
                                          :fixed-names (single-char-syms-from \x)
                                          :rest-name 'args
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
  {:arities (range 1 6)
   :fixed-names (cons 'f (map #(symbol (str "arg" %)) (next (range))))
   :rest-name 'more
   :unrolled-arity (fn [_ [f & fixed-args] rest-args]
                     `(fn ~@(unrolled-fn-tail
                              {:arities (range (if rest-args 0 5))
                               :fixed-names (single-char-syms-from \x)
                               :rest-name 'args
                               :unrolled-arity (fn [_ fixed-additional-args rest-additional-args]
                                                 (maybe-apply f
                                                              (concat fixed-args fixed-additional-args)
                                                              (maybe-concat rest-additional-args rest-args)))})))})


(comment
  (prettify-unrolled (unrolled-fn-tail unrolled-partial-spec))
  (int \x)
  (take 100 (single-char-syms-from \x))
  (take 100 (map char 
                 (drop
                   (- (int \x) (int \a))
                   (cycle (range (int \a) (inc (int \z)))))))
  (int \z)
  (- (int \z) (int \a))
  )
