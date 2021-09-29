;;   Copyright (c) Rich Hickey and contributors. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.non-leaky-macros.clojure.core.async
  "Implementations of clojure.core.async macros that don't leak implementation details."
  (:require [clojure.core.async :as async]))

(defmacro non-leaky-thread
  "Like clojure.core.async/thread, except body cannot access recur target."
  [& body]
  `(async/thread
     (let [res# (do ~@body)]
       res#)))

(defmacro thread
  [& args]
  `(non-leaky-thread ~@args))
