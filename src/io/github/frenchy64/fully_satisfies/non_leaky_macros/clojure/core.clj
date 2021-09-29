;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.non-leaky-macros.clojure.core
  "Implementations of clojure.core macros that don't leak implementation details."
  (:refer-clojure :exclude [locking binding with-bindings sync with-local-vars
                            with-in-str dosync with-precision with-loading-context
                            with-redefs delay vswap! lazy-seq lazy-cat future
                            pvalues])
  (:require [clojure.core :as cc]))

(defmacro non-leaky-locking
  "Like clojure.core/locking, except body cannot leak try/catch syntax."
  [x & body]
  `(cc/locking ~x
     (do ~@body)))

(defmacro locking
  [& args]
  `(non-leaky-locking ~@args))

(defmacro non-leaky-binding
  "Like clojure.core/binding, except body cannot leak try/catch syntax."
  [bindings & body]
  `(cc/binding ~bindings
     (do ~@body)))

(defmacro binding
  [& args]
  `(non-leaky-binding ~@args))

(defmacro non-leaky-with-bindings
  "Like clojure.core/with-bindings, except body cannot leak :pre/post syntax or use (recur)."
  [binding-map & body]
  `(cc/with-bindings ~binding-map
     (let [res# (do ~@body)]
       res#)))

(defmacro with-bindings
  [& args]
  `(non-leaky-with-bindings ~@args))

(defmacro non-leaky-sync
  "Like clojure.core/sync, except body cannot leak pre/post syntax or access
  recur target."
  [flags-ignored-for-now & body]
  `(cc/sync
     ~flags-ignored-for-now
     (let [res# (do ~@body)]
       res#)))

(defmacro sync [& args]
  `(non-leaky-sync ~@args))

(defmacro non-leaky-with-local-vars
  "Like clojure.core/with-local-vars, except body cannot leak try/catch syntax."
  [name-vals-vec & body]
  `(cc/with-local-vars ~name-vals-vec
     (do ~@body)))

(defmacro with-local-vars
  [& args]
  `(non-leaky-with-local-vars ~@args))

(defmacro non-leaky-with-in-str
  "Like clojure.core/with-in-str, except body cannot leak try/catch syntax."
  [s & args]
  `(cc/with-in-str ~s
     (do ~@args)))

(defmacro with-in-str
  [& args]
  `(non-leaky-with-in-str ~@args))

(defmacro non-leaky-dosync
  "Like clojure.core/dosync, except body cannot leak pre/post syntax or use
  recur target."
  [& exprs]
  `(cc/dosync
     (let [res# (do ~@exprs)]
       res#)))

(defmacro dosync
  [& exprs]
  `(non-leaky-dosync ~@exprs))

(defmacro non-leaky-with-precision
  "Like clojure.core/with-precision, except body is cannot leak try/catch syntax."
  [precision & exprs]
    (let [[body rm] (if (= (first exprs) :rounding)
                      [(next (next exprs))
                       `((. java.math.RoundingMode ~(second exprs)))]
                      [exprs nil])]
      `(non-leaky-binding [*math-context* (java.math.MathContext. ~precision ~@rm)]
         ~@body)))

(defmacro with-precision
  [& args]
  `(non-leaky-with-precision ~@args))

(defmacro non-leaky-with-loading-context
  "Like clojure.core/with-loading-context, except body cannot leak try/catch syntax."
  [& body]
  `(cc/with-loading-context
     (do ~@body)))

(defmacro with-loading-context
  [& args]
  `(non-leaky-with-loading-context ~@args))

(defmacro non-leaky-with-redefs
  "Like clojure.core/with-redefs, except body not leak pre/post syntax and does not
  have a recur target available."
  [bindings & body]
  `(cc/with-redefs ~bindings
     (let [res# (do ~@body)]
       res#)))

(defmacro with-redefs
  [& args]
  `(non-leaky-with-redefs ~@args))

(defmacro non-leaky-delay
  "Like clojure.core/delay, except body does not have access to recur target."
  [& body]
  `(cc/delay
     (let [res# (do ~@body)]
       res#)))

(defmacro delay
  [& body]
  `(non-leaky-delay ~@body))

(defmacro non-leaky-vswap!
  "Like clojure.core/with-redefs, except vol is only expanded once and .reset never reflects."
  [vol f & args]
  `(let [v# ~vol]
     (cc/vswap! v# ~f ~@args)))

(defmacro vswap!
  [& args]
  `(non-leaky-vswap! ~@args))

(defmacro non-leaky-lazy-seq
  "Like clojure.core/lazy-seq, except body does not have access to a recur target."
  [& args]
  `(cc/lazy-seq
     (let [res# (do ~@args)]
       res#)))

(defmacro lazy-seq
  [& args]
  `(non-leaky-lazy-seq ~@args))

(defmacro non-leaky-lazy-cat
  "Like clojure.core/lazy-cat, except body does not have access to a recur target."
  [& colls]
  `(concat ~@(map #(list `non-leaky-lazy-seq %) colls)))

(defmacro lazy-cat
  [& args]
  `(non-leaky-lazy-cat ~@args))

(defmacro non-leaky-future
  "Like clojure.core/future, except body does not have access to a recur target."
  [& body]
  `(cc/future
     (let [res# (do ~@body)]
       res#)))

(defmacro future
  [& body]
  `(non-leaky-future ~@body))

(defmacro non-leaky-pvalues
  "Like clojure.core/pvalues, except exprs don't have access to recur targets."
  [& exprs]
  `(pcalls ~@(map #(list `fn [] `(let [res# ~%] res#)) exprs)))

(defmacro pvalues
  [& exprs]
  `(non-leaky-pvalues ~@exprs))
