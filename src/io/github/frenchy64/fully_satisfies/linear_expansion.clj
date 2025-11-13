;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.linear-expansion
  "Macros whose expansions grow linearly rather than exponentially."
  (:refer-clojure :exclude [doseq for])
  (:require [io.github.frenchy64.fully-satisfies.configurable-core-macros.assert-args :refer [assert-args]]))

(defmacro doseq
  "Repeatedly executes body (presumably for side-effects) with
  bindings and filtering as provided by \"for\".  Does not retain
  the head of the sequence. Returns nil.
  
  Unlike clojure.core/doseq, expansion grows linearly with the number
  of nestings of this macro, rather than exponentially."
  [seq-exprs & body]
  (assert-args
    (vector? seq-exprs) "a vector for its binding"
    (even? (count seq-exprs)) "an even number of forms in binding vector")
  (let [step (fn step [recform exprs]
               (if-not exprs
                 [true `(do ~@body)]
                 (let [k (first exprs)
                       v (second exprs)]
                   (if (keyword? k)
                     (let [steppair (step recform (nnext exprs))
                           needrec (steppair 0)
                           subform (steppair 1)]
                       (cond
                         (= k :let) [needrec `(let ~v ~subform)]
                         (= k :while) [false `(when ~v
                                                ~subform
                                                ~@(when needrec [recform]))]
                         (= k :when) [false `(if ~v
                                               (do
                                                 ~subform
                                                 ~@(when needrec [recform]))
                                               ~recform)]))
                     (let [seq- (gensym "seq_")
                           chunk- (with-meta (gensym "chunk_")
                                             {:tag 'clojure.lang.IChunk})
                           count- (gensym "count_")
                           i- (gensym "i_")
                           in-chunk- (gensym "in-chunk_")
                           recform `(if ~in-chunk-
                                      (recur ~seq- ~chunk- ~count- (unchecked-inc ~i-))
                                      (recur (next ~seq-) nil 0 0))
                           steppair (step recform (nnext exprs))
                           needrec (steppair 0)
                           subform (steppair 1)]
                       [true
                        `(loop [~seq- (seq ~v), ~chunk- nil,
                                ~count- 0, ~i- 0]
                           (let [~in-chunk- (< ~i- ~count-)
                                 ~seq- (if ~in-chunk- ~seq- (seq ~seq-))]
                             (when (if ~in-chunk- true ~seq-)
                               (let [chunked?# (if ~in-chunk- false (chunked-seq? ~seq-))
                                     ~k (if ~in-chunk-
                                          (.nth ~chunk- ~i-)
                                          (if chunked?# nil (first ~seq-)))]
                                 (if (if ~in-chunk- false chunked?#)
                                   (let [c# (chunk-first ~seq-)]
                                     (recur (chunk-rest ~seq-) c#
                                            (int (count c#)) (int 0)))
                                   (do ~subform
                                       ~@(when needrec [recform])))))))])))))]
    (nth (step nil (seq seq-exprs)) 1)))

(defmacro for
  "List comprehension. Takes a vector of one or more
   binding-form/collection-expr pairs, each followed by zero or more
   modifiers, and yields a lazy sequence of evaluations of expr.
   Collections are iterated in a nested fashion, rightmost fastest,
   and nested coll-exprs can refer to bindings created in prior
   binding-forms.  Supported modifiers are: :let [binding-form expr ...],
   :while test, :when test.

  (take 100 (for [x (range 100000000) y (range 1000000) :while (< y x)] [x y]))"
  {:added "1.0"}
  [seq-exprs body-expr]
  (assert-args
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")
  (let [to-groups (fn [seq-exprs]
                    (reduce (fn [groups [k v]]
                              (if (keyword? k)
                                (conj (pop groups) (conj (peek groups) [k v]))
                                (conj groups [k v])))
                            [] (partition 2 seq-exprs)))
        err (fn [& msg] (throw (IllegalArgumentException. ^String (apply str msg))))
        gbody (gensym "body__")
        emit-bind (fn emit-bind [[[bind expr & mod-pairs]
                                  & [[_ next-expr] :as next-groups]]]
                    (let [giter (gensym "iter__")
                          gxs (gensym "to-process__")
                          gi (gensym "chunk-index__")
                          gb (gensym "chunk-buffer__")
                          gchunked? (gensym "c?__")
                          gchunk (with-meta (gensym "chunk__") {:tag 'clojure.lang.IChunk})
                          gchunk-size (gensym "csize__")
                          gchunk-iter? (gensym "citer__")
                          outer-loop (boolean next-groups)
                          do-mod (fn do-mod [[[k v :as pair] & etc]]
                                   (cond
                                     (= k :let) `(let ~v ~(do-mod etc))
                                     (= k :while) `(when ~v ~(do-mod etc))
                                     (= k :when) `(if ~v
                                                    ~(do-mod etc)
                                                    (recur (rest ~gxs)))
                                     (keyword? k) (err "Invalid 'for' keyword " k)
                                     outer-loop
                                     `(let [iterys# ~(emit-bind next-groups)
                                            fs# (seq (iterys# ~next-expr))]
                                        (if fs#
                                          (concat fs# (~giter (rest ~gxs)))
                                          (recur (rest ~gxs))))
                                     :else `(cons ~body-expr
                                                  (~giter (rest ~gxs)))))
                          do-cmod (fn do-cmod [[[k v :as pair] & etc]]
                                    (assert (not outer-loop))
                                    (cond
                                      (= k :let) `(let ~v ~(do-cmod etc))
                                      (= k :while) `(if ~v
                                                      ~(do-cmod etc)
                                                      (when ~gchunked?
                                                        ; (not (< ~gi ~gchunk-size)) to break loop
                                                        (recur
                                                          ~gxs
                                                          ~gchunk-size
                                                          ~gchunk-size
                                                          ~gchunk
                                                          false ;; drop results
                                                          ~gb
                                                          ~gchunked?)))
                                      (= k :when) `(if ~v
                                                     ~(do-cmod etc)
                                                     (if ~gchunked?
                                                       (recur
                                                         ~gxs
                                                         (unchecked-inc ~gi)
                                                         ~gchunk-size
                                                         ~gchunk
                                                         ~gchunk-iter?
                                                         ~gb
                                                         ~gchunked?)
                                                       (recur
                                                         (rest ~gxs)
                                                         ~gi
                                                         ~gchunk-size
                                                         ~gchunk
                                                         ~gchunk-iter?
                                                         ~gb
                                                         ~gchunked?)))
                                      (keyword? k) (err "Invalid 'for' keyword " k)
                                      :else `(let [~gbody ~body-expr]
                                               (if ~gchunked?
                                                 (do (chunk-append ~gb ~gbody)
                                                     (recur ~gxs
                                                            (unchecked-inc ~gi)
                                                            ~gchunk-size
                                                            ~gchunk
                                                            ~gchunk-iter?
                                                            ~gb
                                                            ~gchunked?))
                                                 (cons ~gbody
                                                       (~giter (rest ~gxs)))))))]
                      `(fn ~giter [~gxs]
                         (lazy-seq
                           ~(if outer-loop
                              `(loop [~gxs ~gxs]
                                 (when-first [~bind ~gxs]
                                   ~(do-mod mod-pairs)))
                              `(loop [~gxs ~gxs
                                      ~gi (int 0)
                                      ~gchunk-size (int 1) ;; before newly-chunked, ensure (< ~gi ~gchunk-size)
                                      ~gchunk nil
                                      ~gchunk-iter? true
                                      ~gb nil
                                      ~gchunked? false]
                                 (if (< ~gi ~gchunk-size)
                                   (when-let [~gxs (if ~gchunked? ~gxs (seq ~gxs))]
                                     (let [chunked# (chunked-seq? ~gxs)
                                           newly-chunked?# (if ~gchunked? false chunked#)
                                           ~gchunked? (or ~gchunked? chunked#)
                                           ~gi (if newly-chunked?# (int 0) ~gi)
                                           ~gchunk (if newly-chunked?# (chunk-first ~gxs) ~gchunk)
                                           ~gchunk-size (if newly-chunked?#
                                                          (int (count ~gchunk))
                                                          ~gchunk-size)
                                           ~gb (if newly-chunked?# (chunk-buffer ~gchunk-size) ~gb)
                                           ~bind (if ~gchunked?
                                                   (.nth ~gchunk ~gi)
                                                   (first ~gxs))]
                                       ~(do-cmod mod-pairs)))
                                   (if ~gchunk-iter?
                                     (chunk-cons
                                       (chunk ~gb)
                                       (~giter (chunk-rest ~gxs)))
                                     (chunk-cons (chunk ~gb) nil)))))))))]
  `(let [iter# ~(emit-bind (to-groups seq-exprs))]
     (iter# ~(second seq-exprs)))))
