;; Copyright (c) Stuart Halloway. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.non-leaky-macros.clojure.core.match.debug
  "Implementations of clojure.core.match.debug macros that don't leak implementation details."
  (:require [clojure.core.match.debug :as dbg]))

(defmacro non-leaky-with-recur
  "Like clojure.core.match.debug/with-recur, except body does not leak try/catch syntax."
  [form]
  `(dbg/with-recur (do ~form)))

(defmacro with-recur [& args]
  `(non-leaky-with-recur ~@args))
