;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.hygienic
  (:refer-clojure :exclude [locking binding with-bindings sync with-local-vars
                            with-in-str dosync with-precision with-loading-context
                            with-redefs delay vswap! lazy-seq lazy-cat future
                            pvalues])
  (:require [clojure.core :as cc]))

(defmacro hygienic-locking
  "Like clojure.core/locking, except body is expanded hygienically. Body
  cannot leak try/catch syntax."
  [x & body]
  `(cc/locking ~x
     (do ~@body)))

(defmacro locking
  [& args]
  `(hygienic-locking ~@args))

(defmacro hygienic-binding
  "Like clojure.core/binding, except body is expanded hygienically. Body
  cannot leak try/catch syntax."
  [bindings & body]
  `(cc/binding ~bindings
     (do ~@body)))

(defmacro binding
  [& args]
  `(hygienic-binding ~@args))

(defmacro hygienic-with-bindings
  "Like clojure.core/with-bindings, except body is expanded hygienically. Body
  cannot leak :pre/post syntax or use (recur)."
  [binding-map & body]
  `(cc/with-bindings ~binding-map
     (let [res# (do ~@body)]
       res#)))

(defmacro with-bindings
  [& args]
  `(hygienic-with-bindings ~@args))

(defmacro hygienic-sync
  "Like clojure.core/sync, except body is expanded hygienically. Body
  cannot leak pre/post syntax or use (recur)."
  [flags-ignored-for-now & body]
  `(cc/sync
     ~flags-ignored-for-now
     (let [res# (do ~@body)]
       res#)))

(defmacro sync [& args]
  `(hygienic-sync ~@args))

(defmacro hygienic-with-local-vars
  "Like clojure.core/with-local-vars, except body is expanded hygienically. Body
  cannot leak try/catch syntax."
  [name-vals-vec & body]
  `(cc/with-local-vars ~name-vals-vec
     (do ~@body)))

(defmacro with-local-vars
  [& args]
  `(hygienic-with-local-vars ~@args))

(defmacro hygienic-with-in-str
  "Like clojure.core/with-in-str, except body is expanded hygienically."
  [s & args]
  `(cc/with-in-str ~s
     (do ~@args)))

(defmacro with-in-str
  [& args]
  `(hygienic-with-in-str ~@args))

(defmacro hygienic-dosync
  "Like clojure.core/dosync, except body is expanded hygienically. Body
  cannot leak pre/post syntax or use (recur)"
  [& exprs]
  `(cc/dosync
     (let [res# (do ~@exprs)]
       res#)))

(defmacro dosync
  [& exprs]
  `(hygienic-dosync ~@exprs))

(defmacro hygienic-with-precision
  "Like clojure.core/with-precision, except body is expanded hygienically."
  [precision & exprs]
    (let [[body rm] (if (= (first exprs) :rounding)
                      [(next (next exprs))
                       `((. java.math.RoundingMode ~(second exprs)))]
                      [exprs nil])]
      `(hygienic-binding [*math-context* (java.math.MathContext. ~precision ~@rm)]
         ~@body)))

(defmacro with-precision
  [& args]
  `(hygienic-with-precision ~@args))

(defmacro hygienic-with-loading-context
  "Like clojure.core/with-loading-context, except body is expanded hygienically."
  [& body]
  `(cc/with-loading-context
     (do ~@body)))

(defmacro with-loading-context
  [& args]
  `(hygienic-with-loading-context ~@args))

(defmacro hygienic-with-redefs
  "Like clojure.core/with-redefs, except body is expanded hygienically. Body does
  not leak pre/post syntax and does not have a recur target available."
  [bindings & body]
  `(cc/with-redefs ~bindings
     (let [res# (do ~@body)]
       res#)))

(defmacro with-redefs
  [& args]
  `(hygienic-with-redefs ~@args))

(defmacro hygienic-delay
  "Like clojure.core/delay, except body is expanded hygienically such that
  `recur`ing into its implementation is not possible."
  [& body]
  `(cc/delay
     (let [res# (do ~@body)]
       res#)))

(defmacro delay
  [& body]
  `(hygienic-delay ~@body))

(defmacro hygienic-vswap!
  "Like clojure.core/with-redefs, except vol is only expanded once
  and .reset never reflects."
  [vol f & args]
  `(let [v# ~vol]
     (cc/vswap! v# ~f ~@args)))

(defmacro vswap!
  [& args]
  `(hygienic-vswap! ~@args))

(defmacro hygienic-lazy-seq
  "Like clojure.core/lazy-seq, except body does not have access to
  a recur target."
  [& args]
  `(cc/lazy-seq
     (let [res# (do ~@args)]
       res#)))

(defmacro lazy-seq
  [& args]
  `(hygienic-lazy-seq ~@args))

(defmacro hygienic-lazy-cat
  "Like clojure.core/lazy-cat, except body does not have access to
  a recur target."
  [& colls]
  `(concat ~@(map #(list `hygienic-lazy-seq %) colls)))

(defmacro lazy-cat
  [& args]
  `(hygienic-lazy-cat ~@args))

(defmacro hygienic-future
  "Like clojure.core/future, except body does not have access to
  a recur target."
  [& body]
  `(cc/future
     (let [res# (do ~@body)]
       res#)))

(defmacro future
  [& body]
  `(hygienic-future ~@body))

(defmacro hygienic-pvalues
  "Like clojure.core/pvalues, except exprs don't have access to
  recur targets."
  [& exprs]
  `(pcalls ~@(map #(list `fn [] `(let [res# ~%] res#)) exprs)))

(defmacro pvalues
  [& exprs]
  `(hygienic-pvalues ~@exprs))
