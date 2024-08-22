(ns
  "io.github.frenchy64.fully-satisfies.configurable-core-macros-test-generated-zprint"
  (:require
    [clojure.core :as cc]
    [io.github.frenchy64.fully-satisfies.configurable-core-macros.let :as let]
    [io.github.frenchy64.fully-satisfies.configurable-core-macros.fn :as fn]
    [io.github.frenchy64.fully-satisfies.configurable-core-macros.defn :as
     defn]))

(clojure.core/defmacro my-let
  "binding => binding-form init-expr\n     binding-form => name, or destructuring-form\n     destructuring-form => map-destructure-form, or seq-destructure-form\n\n     Evaluates the exprs in a lexical context in which the symbols in\n     the binding-forms are bound to their respective init-exprs or parts\n     therein.\n\n     See https://clojure.org/reference/special_forms#binding-forms for\n     more information about destructuring."
  {:forms (quote [(let [bindings*] exprs*)])}
  [bindings__6002__auto__ & body__6003__auto__]
  (io.github.frenchy64.fully-satisfies.configurable-core-macros.let/let-implementation
    bindings__6002__auto__
    body__6003__auto__
    (quote
      {:rename #:clojure.core{let my-let, fn my-fn, defn my-defn},
       :opts-var
         io.github.frenchy64.fully-satisfies.configurable-core-macros-test/opts})))

(clojure.core/defmacro my-fn
  "params => positional-params*, or positional-params* & rest-param\n     positional-param => binding-form\n     rest-param => binding-form\n     binding-form => name, or destructuring-form\n\n     Defines a function.\n\n     See https://clojure.org/reference/special_forms#fn for more information"
  {:forms (quote [(fn name? [params*] exprs*) (fn name? ([params*] exprs*) +)])}
  [& sigs__6032__auto__]
  (io.github.frenchy64.fully-satisfies.configurable-core-macros.fn/fn-implementation
    &form
    sigs__6032__auto__
    (quote
      {:rename #:clojure.core{let my-let, fn my-fn, defn my-defn},
       :opts-var
         io.github.frenchy64.fully-satisfies.configurable-core-macros-test/opts})))

(def my-defn
  (clojure.core/fn my-defn [&form__6065__auto__ &env__6066__auto__
                            name__6067__auto__ & fdecl__6068__auto__]
    (io.github.frenchy64.fully-satisfies.configurable-core-macros.defn/defn-implementation
      &form__6065__auto__
      &env__6066__auto__
      name__6067__auto__
      fdecl__6068__auto__
      {:rename #:clojure.core{let my-let, fn my-fn, defn my-defn},
       :opts-var
         io.github.frenchy64.fully-satisfies.configurable-core-macros-test/opts})))

(clojure.core/doto (var my-defn) .setMacro)