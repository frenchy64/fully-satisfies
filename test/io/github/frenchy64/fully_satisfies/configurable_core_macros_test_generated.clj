(ns io.github.frenchy64.fully-satisfies.configurable-core-macros-test-generated)
(clojure.core/defmacro
 my-let
 "binding => binding-form init-expr\n     binding-form => name, or destructuring-form\n     destructuring-form => map-destructure-form, or seq-destructure-form\n\n     Evaluates the exprs in a lexical context in which the symbols in\n     the binding-forms are bound to their respective init-exprs or parts\n     therein.\n\n     See https://clojure.org/reference/special_forms#binding-forms for\n     more information about destructuring."
 {:forms '[^{:line 37, :column 60} (let [bindings*] exprs*)]}
 [bindings__2337__auto__ & body__2338__auto__]
 (io.github.frenchy64.fully-satisfies.configurable-core-macros.let/let-implementation
  bindings__2337__auto__
  body__2338__auto__
  'io.github.frenchy64.fully-satisfies.configurable-core-macros-test/opts))
(clojure.core/defmacro
 my-fn
 "params => positional-params*, or positional-params* & rest-param\n     positional-param => binding-form\n     rest-param => binding-form\n     binding-form => name, or destructuring-form\n\n     Defines a function.\n\n     See https://clojure.org/reference/special_forms#fn for more information"
 {:forms
  '[^{:line 103, :column 18}
    (fn name? [params*] exprs*)
    ^{:line 103, :column 47}
    (fn name? ^{:line 103, :column 57} ([params*] exprs*) +)]}
 [& sigs__2189__auto__]
 (io.github.frenchy64.fully-satisfies.configurable-core-macros.fn/fn-implementation
  &form
  sigs__2189__auto__
  'io.github.frenchy64.fully-satisfies.configurable-core-macros-test/opts))
(def
 ^{:doc "Same as (def name (fn [params* ] exprs*)) or (def\n                             name (fn ([params* ] exprs*)+)) with any doc-string or attrs added\n                             to the var metadata. prepost-map defines a map with optional keys\n                             :pre and :post that contain collections of pre or post conditions.", :arglists (quote ^{:line 106, :column 37} ([name doc-string? attr-map? [params*] prepost-map? body] [name doc-string? attr-map? ^{:line 107, :column 66} ([params*] prepost-map? body) + attr-map?]))} my-defn
 (clojure.core/fn
  my-defn
  [&form__3371__auto__
   &env__3372__auto__
   name__3373__auto__
   &
   fdecl__3374__auto__]
  (io.github.frenchy64.fully-satisfies.configurable-core-macros.defn/defn-implementation
   &form__3371__auto__
   &env__3372__auto__
   name__3373__auto__
   fdecl__3374__auto__
   'io.github.frenchy64.fully-satisfies.configurable-core-macros-test/opts)))
(clojure.core/doto #'my-defn .setMacro)
