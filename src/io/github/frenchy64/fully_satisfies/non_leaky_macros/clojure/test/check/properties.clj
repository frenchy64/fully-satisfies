;   Copyright (c) Rich Hickey, Reid Draper, and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.non-leaky-macros.clojure.test.check.properties
  "Implementations of clojure.test.check.properties macros that don't leak implementation details."
  (:require [clojure.test.check.properties :as prop]))

(defmacro non-leaky-for-all
  "Like clojure.test.check.properties/for-all, except body does not have a recur target available."
  [bindings & body]
  `(prop/for-all
     ~bindings
     (let [res# (do ~@body)]
       res#)))

(defmacro for-all
  [& args]
  `(non-leaky-for-all ~@args))
