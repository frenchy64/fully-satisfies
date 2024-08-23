;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.configurable-core-macros.if-let
  (:require [clojure.core :as cc]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.utils :as u]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.assert-args :as assert-args]))

;;;;;;;;;;;;;;;;
;; if-let
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
(defn if-let-implementation
  ([&form bindings then opts]
   (if-let-implementation &form bindings then nil nil opts))
  ([&form bindings then else oldform opts]
   (assert-args/assert-args ;;uses &form
     (vector? bindings) "a vector for its binding"
     (nil? oldform) "1 or 2 forms after binding vector"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
   (let [form (bindings 0) tst (bindings 1)]
     `(let [temp# ~tst]
        (if temp#
          (~(u/replacement-for `let opts) [~form temp#]
            ~then)
          ~else)))))

(defmacro ->if-let [opts]
  (let [macro-name (u/rename-to `if-let opts)]
    `(defmacro ~(u/rename-to `if-let opts)
       "bindings => binding-form test

       If test is true, evaluates then with binding-form bound to the value of 
       test, if not, yields else"
       {:added "1.0"
        :arglists '~'([bindings then] [bindings then else #_#_& oldform])}
       ([bindings# then#]
        (if-let-implementation ~'&form bindings# then# '~opts))
       ([bindings# then# else# & oldform#]
        (if-let-implementation ~'&form bindings# then# else# oldform# '~opts)))))
