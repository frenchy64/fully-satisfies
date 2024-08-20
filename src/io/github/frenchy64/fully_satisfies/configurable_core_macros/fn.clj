;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.configurable-core-macros.fn
  (:require [clojure.core :as cc]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.utils :as u]))

;;;;;;;;;;;;;;;;
;; fn
;;;;;;;;;;;;;;;;

(def info {`let {:dependencies #{`let `assert}}})

(defn- maybe-destructured
  [params body opts]
  (if (every? symbol? params)
    (cons params body)
    (loop [params params
           new-params (with-meta [] (meta params))
           lets []]
      (if params
        (if (symbol? (first params))
          (recur (next params) (conj new-params (first params)) lets)
          (let [gparam (gensym "p__")]
            (recur (next params) (conj new-params gparam)
                   (-> lets (conj (first params)) (conj gparam)))))
        `(~new-params
          (~(u/replacement-for `let opts) ~lets
            ~@body))))))

;; internal
(defn fn-implementation
  [&form sigs opts]
  (let [name (if (symbol? (first sigs)) (first sigs) nil)
        sigs (if name (next sigs) sigs)
        sigs (if (vector? (first sigs))
               (list sigs)
               (if (seq? (first sigs))
                 sigs
                 ;; Assume single arity syntax
                 (throw (IllegalArgumentException.
                          (if (seq sigs)
                            (str "Parameter declaration "
                                 (first sigs)
                                 " should be a vector")
                            (str "Parameter declaration missing"))))))
        psig (fn* [sig]
               ;; Ensure correct type before destructuring sig
               (when (not (seq? sig))
                 (throw (IllegalArgumentException.
                          (str "Invalid signature " sig
                               " should be a list"))))
               (let [[params & body] sig
                     _ (when (not (vector? params))
                         (throw (IllegalArgumentException.
                                  (if (seq? (first sigs))
                                    (str "Parameter declaration " params
                                         " should be a vector")
                                    (str "Invalid signature " sig
                                         " should be a list")))))
                     conds (when (and (next body) (map? (first body)))
                             (first body))
                     body (if conds (next body) body)
                     conds (or conds (meta params))
                     pre (:pre conds)
                     post (:post conds)
                     body (if post
                            `((let [~'% ~(if (< 1 (count body)) 
                                          `(do ~@body) 
                                          (first body))]
                               ~@(map (fn* [c] `(~(u/replacement-for `assert opts) ~c)) post)
                               ~'%))
                            body)
                     body (if pre
                            (concat (map (fn* [c] `(~(u/replacement-for `assert opts) ~c)) pre) 
                                    body)
                            body)]
                 (maybe-destructured params body opts)))
        new-sigs (map psig sigs)]
    (with-meta
      (if name
        (list* 'fn* name new-sigs)
        (cons 'fn* new-sigs))
      (meta &form))))


(defmacro ->fn [opts]
  `(defmacro ~(u/rename-to 'fn opts)
     "params => positional-params*, or positional-params* & rest-param
     positional-param => binding-form
     rest-param => binding-form
     binding-form => name, or destructuring-form

     Defines a function.

     See https://clojure.org/reference/special_forms#fn for more information"
     {;:added "1.0", :special-form true,
      :forms '~'[(fn name? [params* ] exprs*) (fn name? ([params* ] exprs*)+)]}
     [& sigs#]
     (fn-implementation ~'&form sigs# '~opts)))
