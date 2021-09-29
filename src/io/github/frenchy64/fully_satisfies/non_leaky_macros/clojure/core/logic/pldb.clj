;   Copyright (c) David Nolen, Rich Hickey, contributors. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.non-leaky-macros.clojure.core.logic.pldb
  "Implementations of clojure.core.logic.pldb macros that don't leak implementation details."
  (:require [clojure.core.logic.pldb :as pldb]))

(defmacro non-leaky-with-dbs
  "Like clojure.core.logic.pldb/with-dbs, except body cannot leak try/catch syntax."
  [dbs & body]
  `(pldb/with-dbs ~dbs
     (do ~@body)))

(defmacro with-dbs
  [& args]
  `(non-leaky-with-dbs ~@args))

(defmacro non-leaky-with-db
  "Like clojure.core.logic.pldb/with-db, except body cannot leak try/catch syntax."
  [db & body]
  `(pldb/with-db ~db
     (do ~@body)))

(defmacro with-db
  [& args]
  `(non-leaky-with-db ~@args))
