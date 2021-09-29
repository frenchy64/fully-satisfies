;; Copyright (c) Stuart Halloway. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.non-leaky-macros.clojure.java.jmx
  "Implementations of clojure.java.jmx macros that don't leak implementation details."
  (:require [clojure.java.jmx :as jmx]))

(defmacro non-leaky-with-connection
  "Like clojure.java.jmx/with-connection, except body does not leak try/catch syntax."
  [opts & body]
  `(jmx/with-connection ~opts
     (do ~@body)))

(defmacro with-connection [& args]
  `(non-leaky-with-connection ~@args))
