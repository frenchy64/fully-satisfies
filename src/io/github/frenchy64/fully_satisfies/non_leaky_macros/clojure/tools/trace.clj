;;  Copyright (c) Stuart Sierra, 2008. All rights reserved. The use
;;  and distribution terms for this software are covered by the Eclipse
;;  Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;  which can be found in the file epl-v10.html at the root of this
;;  distribution. By using this software in any fashion, you are
;;  agreeing to be bound by the terms of this license. You must not
;;  remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.non-leaky-macros.clojure.tools.trace
  "Implementations of clojure.tools.trace macros that don't leak implementation details."
  (:require [clojure.tools.trace :as trace]))

(defmacro non-leaky-dotrace
  "Like clojure.tools.trace/dotrace, except body cannot leak try/catch syntax."
  [fnames & exprs]
  `(trace/dotrace ~fnames (do ~@expr)))

(defmacro dotrace
  [& args]
  `(non-leaky-dotrace ~@args))
