;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.non-leaky-macros.clojure.test.tap
  "Implementations of clojure.test.tap macros that don't leak implementation details."
  (:require [clojure.test.tap :as tap]))

(defmacro non-leaky-with-tap-output
  "Like clojure.test.tap/with-tap-output, except body does not leak try/catch syntax."
  [& body]
  `(tap/with-tap-output
     (do ~@body)))

(defmacro with-tap-output
  [& body]
  `(non-leaky-with-tap-output ~@body))
