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
                            with-redefs delay vswap!])
  (:require [clojure.core :as cc]))

(defmacro hygienic-locking
  "Like clojure.core/locking, except body is expanded hygienically."
  [x & body]
  `(cc/locking ~x
     (do ~@body)))

(defmacro locking
  [& args]
  `(hygienic-locking ~@args))

(defmacro hygienic-binding
  "Like clojure.core/binding, except body is expanded hygienically."
  [bindings & body]
  `(cc/binding ~bindings
     (do ~@body)))

(defmacro binding
  [& args]
  `(hygienic-binding ~@args))

(defmacro hygienic-with-bindings
  "Like clojure.core/with-bindings, except body is expanded hygienically."
  [binding-map & body]
  `(cc/with-bindings ~binding-map
     (do ~@body)))

(defmacro with-bindings
  [& args]
  `(hygienic-with-bindings ~@args))

(defmacro hygienic-sync
  "Like clojure.core/sync, except body is expanded hygienically."
  [flags-ignored-for-now & body]
  `(cc/sync
     ~flags-ignored-for-now
     (do ~@body)))

(defmacro sync [& args]
  `(hygienic-sync ~@args))

(defmacro hygienic-with-local-vars
  "Like clojure.core/with-local-vars, except body is expanded hygienically."
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
  "Like clojure.core/dosync, except body is expanded hygienically."
  [& exprs]
  `(cc/dosync
     (do ~@exprs)))

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
  "Like clojure.core/with-redefs, except body is expanded hygienically."
  [bindings & body]
  `(cc/with-redefs ~bindings
     (do ~@body)))

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
  (let [v (with-meta (gensym "vol") {:tag 'clojure.lang.Volatile})]
    `(let [~v ~vol]
       (.reset ~v (~f (.deref ~v) ~@args)))))

(defmacro vswap!
  [& args]
  `(hygienic-vswap! ~@args))
