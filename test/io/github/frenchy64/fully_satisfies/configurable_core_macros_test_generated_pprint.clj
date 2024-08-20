;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;; formatted by :pprint
(ns
 io.github.frenchy64.fully-satisfies.configurable-core-macros-test-generated-pprint
 (:refer-clojure :only [])
 (:require
  [clojure.core :as cc]
  io.github.frenchy64.fully-satisfies.configurable-core-macros.let
  io.github.frenchy64.fully-satisfies.configurable-core-macros.fn
  io.github.frenchy64.fully-satisfies.configurable-core-macros.defn
  io.github.frenchy64.fully-satisfies.configurable-core-macros.defmacro
  io.github.frenchy64.fully-satisfies.configurable-core-macros.defmethod
  io.github.frenchy64.fully-satisfies.configurable-core-macros.if-let))

(clojure.core/defmacro my-let
  "binding => binding-form init-expr\n     binding-form => name, or destructuring-form\n     destructuring-form => map-destructure-form, or seq-destructure-form\n\n     Evaluates the exprs in a lexical context in which the symbols in\n     the binding-forms are bound to their respective init-exprs or parts\n     therein.\n\n     See https://clojure.org/reference/special_forms#binding-forms for\n     more information about destructuring."
  {:forms '[(let [bindings*] exprs*)]}
  [bindings__16012__auto__ & body__16013__auto__]
  (io.github.frenchy64.fully-satisfies.configurable-core-macros.let/let-implementation
    &form
    bindings__16012__auto__
    body__16013__auto__
    '{:rename
      {clojure.core/let my-let,
       clojure.core/fn my-fn,
       clojure.core/defn my-defn,
       clojure.core/defmacro my-defmacro,
       clojure.core/defmethod my-defmethod,
       clojure.core/if-let my-if-let},
      :opts-var
      io.github.frenchy64.fully-satisfies.configurable-core-macros-test/opts}))

(clojure.core/defmacro my-fn
  "params => positional-params*, or positional-params* & rest-param\n     positional-param => binding-form\n     rest-param => binding-form\n     binding-form => name, or destructuring-form\n\n     Defines a function.\n\n     See https://clojure.org/reference/special_forms#fn for more information"
  {:forms
   '[(fn name? [params*] exprs*) (fn name? ([params*] exprs*) +)]}
  [& sigs__15093__auto__]
  (io.github.frenchy64.fully-satisfies.configurable-core-macros.fn/fn-implementation
    &form
    sigs__15093__auto__
    '{:rename
      {clojure.core/let my-let,
       clojure.core/fn my-fn,
       clojure.core/defn my-defn,
       clojure.core/defmacro my-defmacro,
       clojure.core/defmethod my-defmethod,
       clojure.core/if-let my-if-let},
      :opts-var
      io.github.frenchy64.fully-satisfies.configurable-core-macros-test/opts}))

(def ^{:doc "Same as (def name (fn [params* ] exprs*)) or (def\n                             name (fn ([params* ] exprs*)+)) with any doc-string or attrs added\n                             to the var metadata. prepost-map defines a map with optional keys\n                             :pre and :post that contain collections of pre or post conditions.", :arglists (quote ([name doc-string? attr-map? [params*] prepost-map? body] [name doc-string? attr-map? ([params*] prepost-map? body) + attr-map?]))} my-defn
 (clojure.core/fn my-defn [&form__15126__auto__
                           &env__15127__auto__
                           name__15128__auto__
                           &
                           fdecl__15129__auto__]
   (io.github.frenchy64.fully-satisfies.configurable-core-macros.defn/defn-implementation
     name__15128__auto__
     fdecl__15129__auto__
     false
     '{:rename
       {clojure.core/let my-let,
        clojure.core/fn my-fn,
        clojure.core/defn my-defn,
        clojure.core/defmacro my-defmacro,
        clojure.core/defmethod my-defmethod,
        clojure.core/if-let my-if-let},
       :opts-var
       io.github.frenchy64.fully-satisfies.configurable-core-macros-test/opts})))

(clojure.core/doto #'my-defn .setMacro)

(def ^{:doc "Same as (def name (fn [params* ] exprs*)) or (def\n                             name (fn ([params* ] exprs*)+)) with any doc-string or attrs added\n                             to the var metadata. prepost-map defines a map with optional keys\n                             :pre and :post that contain collections of pre or post conditions.", :arglists (quote ([name doc-string? attr-map? [params*] prepost-map? body] [name doc-string? attr-map? ([params*] prepost-map? body) + attr-map?]))} my-defn
 (clojure.core/fn my-defn [&form__15126__auto__
                           &env__15127__auto__
                           name__15128__auto__
                           &
                           fdecl__15129__auto__]
   (io.github.frenchy64.fully-satisfies.configurable-core-macros.defn/defn-implementation
     name__15128__auto__
     fdecl__15129__auto__
     false
     '{:rename
       {clojure.core/let my-let,
        clojure.core/fn my-fn,
        clojure.core/defn my-defn,
        clojure.core/defmacro my-defmacro,
        clojure.core/defmethod my-defmethod,
        clojure.core/if-let my-if-let},
       :opts-var
       io.github.frenchy64.fully-satisfies.configurable-core-macros-test/opts})))

(clojure.core/doto #'my-defn .setMacro)

(clojure.core/defmacro my-defmethod
  "Creates and installs a new method of multimethod associated with dispatch-value. "
  {:arglists '([multifn dispatch-val & fn-tail])}
  [multifn__15176__auto__
   dispatch-val__15177__auto__
   &
   fn-tail__15178__auto__]
  (io.github.frenchy64.fully-satisfies.configurable-core-macros.defmethod/defmethod-implementation
    multifn__15176__auto__
    dispatch-val__15177__auto__
    fn-tail__15178__auto__
    '{:rename
      {clojure.core/let my-let,
       clojure.core/fn my-fn,
       clojure.core/defn my-defn,
       clojure.core/defmacro my-defmacro,
       clojure.core/defmethod my-defmethod,
       clojure.core/if-let my-if-let},
      :opts-var
      io.github.frenchy64.fully-satisfies.configurable-core-macros-test/opts}))

(clojure.core/defmacro my-if-let
  "bindings => binding-form test\n\n     If test is true, evaluates then with binding-form bound to the value of \n     test, if not, yields else"
  {:added "1.0", :arglists '([bindings then] [bindings then else])}
  ([bindings__2227__auto__ then__2228__auto__]
    (io.github.frenchy64.fully-satisfies.configurable-core-macros.if-let/if-let-implementation
      &form
      bindings__2227__auto__
      then__2228__auto__
      '{:rename
        {clojure.core/let my-let,
         clojure.core/fn my-fn,
         clojure.core/defn my-defn,
         clojure.core/defmacro my-defmacro,
         clojure.core/defmethod my-defmethod,
         clojure.core/if-let my-if-let},
        :opts-var
        io.github.frenchy64.fully-satisfies.configurable-core-macros-test/opts}))
  ([bindings__2227__auto__
    then__2228__auto__
    else__2229__auto__
    &
    oldform__2230__auto__]
    (io.github.frenchy64.fully-satisfies.configurable-core-macros.if-let/if-let-implementation
      &form
      bindings__2227__auto__
      then__2228__auto__
      else__2229__auto__
      oldform__2230__auto__
      '{:rename
        {clojure.core/let my-let,
         clojure.core/fn my-fn,
         clojure.core/defn my-defn,
         clojure.core/defmacro my-defmacro,
         clojure.core/defmethod my-defmethod,
         clojure.core/if-let my-if-let},
        :opts-var
        io.github.frenchy64.fully-satisfies.configurable-core-macros-test/opts})))
