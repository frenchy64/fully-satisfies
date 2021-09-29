;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.non-leaky-macros.clojure.pprint
  "Implementations of clojure.pprint macros that don't leak implementation details."
  (:require [clojure.pprint :as pp]))

(defmacro non-leaky-with-pprint-dispatch
  "Like clojure.pprint/with-pprint-dispatch, except body does not leak try/catch syntax."
  [function & body]
  `(pp/with-pprint-dispatch
     ~function
     (do ~@body)))

(defmacro with-pprint-dispatch
  [& args]
  `(non-leaky-with-pprint-dispatch ~@args))
