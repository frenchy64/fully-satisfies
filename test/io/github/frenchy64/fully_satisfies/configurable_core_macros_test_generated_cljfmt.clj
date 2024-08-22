;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;; formatted by :cljfmt
(ns
 io.github.frenchy64.fully-satisfies.configurable-core-macros-test-generated-cljfmt
  ^{:line 99, :column 39}
  (:refer-clojure :only [])
  (:require
   [clojure.core :as cc]
   [io.github.frenchy64.fully-satisfies.configurable-core-macros.let
    :as
    let]
   [io.github.frenchy64.fully-satisfies.configurable-core-macros.fn
    :as
    fn]
   [io.github.frenchy64.fully-satisfies.configurable-core-macros.defn
    :as
    defn]))

(clojure.core/defmacro
  my-let
  "binding => binding-form init-expr\n     binding-form => name, or destructuring-form\n     destructuring-form => map-destructure-form, or seq-destructure-form\n\n     Evaluates the exprs in a lexical context in which the symbols in\n     the binding-forms are bound to their respective init-exprs or parts\n     therein.\n\n     See https://clojure.org/reference/special_forms#binding-forms for\n     more information about destructuring."
  {:forms '[^{:line 37, :column 60} (let [bindings*] exprs*)]}
  [bindings__2129__auto__ & body__2130__auto__]
  (io.github.frenchy64.fully-satisfies.configurable-core-macros.let/let-implementation
   bindings__2129__auto__
   body__2130__auto__
   '{:rename
     {clojure.core/let my-let,
      clojure.core/fn my-fn,
      clojure.core/defn my-defn},
     :opts-var
     io.github.frenchy64.fully-satisfies.configurable-core-macros-test/opts}))

(clojure.core/defmacro
  my-fn
  "params => positional-params*, or positional-params* & rest-param\n     positional-param => binding-form\n     rest-param => binding-form\n     binding-form => name, or destructuring-form\n\n     Defines a function.\n\n     See https://clojure.org/reference/special_forms#fn for more information"
  {:forms
   '[^{:line 102, :column 18}
     (fn name? [params*] exprs*)
     ^{:line 102, :column 47}
     (fn name? ^{:line 102, :column 57} ([params*] exprs*) +)]}
  [& sigs__2159__auto__]
  (io.github.frenchy64.fully-satisfies.configurable-core-macros.fn/fn-implementation
   &form
   sigs__2159__auto__
   '{:rename
     {clojure.core/let my-let,
      clojure.core/fn my-fn,
      clojure.core/defn my-defn},
     :opts-var
     io.github.frenchy64.fully-satisfies.configurable-core-macros-test/opts}))

(def
  ^{:doc "Same as (def name (fn [params* ] exprs*)) or (def\n                             name (fn ([params* ] exprs*)+)) with any doc-string or attrs added\n                             to the var metadata. prepost-map defines a map with optional keys\n                             :pre and :post that contain collections of pre or post conditions.", :arglists (quote ^{:line 107, :column 37} ([name doc-string? attr-map? [params*] prepost-map? body] [name doc-string? attr-map? ^{:line 108, :column 66} ([params*] prepost-map? body) + attr-map?]))} my-defn
  (clojure.core/fn
    my-defn
    [&form__2192__auto__
     &env__2193__auto__
     name__2194__auto__
     &
     fdecl__2195__auto__]
    (io.github.frenchy64.fully-satisfies.configurable-core-macros.defn/defn-implementation
      &form__2192__auto__
      &env__2193__auto__
      name__2194__auto__
      fdecl__2195__auto__
      '{:rename
        {clojure.core/let my-let,
         clojure.core/fn my-fn,
         clojure.core/defn my-defn},
        :opts-var
        io.github.frenchy64.fully-satisfies.configurable-core-macros-test/opts})))

(clojure.core/doto #'my-defn .setMacro)
