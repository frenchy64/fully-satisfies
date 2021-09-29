;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.non-leaky-macros.clojure.java.shell
  "Implementations of clojure.java.test macros that don't leak implementation details."
  (:require [clojure.java.shell :as sh]))

(defmacro non-leaky-with-sh-dir
  "Like clojure.java.shell/with-sh-dir, except body does not leak try/catch syntax."
  [dir & forms]
  `(sh/with-sh-dir ~dir
     (do ~@forms)))

(defmacro with-sh-dir
  [& args]
  `(non-leaky-with-sh-dir ~@args))

(defmacro non-leaky-with-sh-env
  "Like clojure.java.shell/with-sh-env, except body does not leak try/catch syntax."
  [env & forms]
  `(sh/with-sh-env ~env
     (do ~@forms)))

(defmacro with-sh-env
  [& args]
  `(non-leaky-with-sh-env ~@args))
