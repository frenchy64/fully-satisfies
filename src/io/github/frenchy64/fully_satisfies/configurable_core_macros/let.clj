;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.configurable-core-macros.let
  (:require [clojure.core :as cc]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.utils :as u]))

;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.core/let
;;;;;;;;;;;;;;;;;;;;;;;

(def info {`let {:dependencies #{`destructure}}})

(defn let-implementation [bindings body options]
  (#'clojure.core/assert-args
    (vector? bindings) "a vector for its binding"
    (even? (count bindings)) "an even number of forms in binding vector")
  `(let* ~((u/replacement-for #'destructure options) bindings) ~@body))

(defmacro ->let [opts]
  `(defmacro ~(u/rename-to 'let opts)
     "binding => binding-form init-expr
     binding-form => name, or destructuring-form
     destructuring-form => map-destructure-form, or seq-destructure-form

     Evaluates the exprs in a lexical context in which the symbols in
     the binding-forms are bound to their respective init-exprs or parts
     therein.

     See https://clojure.org/reference/special_forms#binding-forms for
     more information about destructuring."
     {#_#_:added "1.0", #_#_:special-form true, :forms '~'[(let [bindings*] exprs*)]}
     [bindings# & body#]
     (let-implementation bindings# body# '~opts)))
