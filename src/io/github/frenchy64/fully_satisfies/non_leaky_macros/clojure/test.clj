;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.non-leaky-macros.clojure.test
  "Implementations of clojure.test macros that don't leak implementation details."
  (:require [clojure.test :as ct]))

(defmacro non-leaky-with-test-out
  "Like clojure.test/with-test-out, except body does not leak try/catch syntax."
  [& body]
  `(ct/with-test-out
     (do ~@body)))

(defmacro with-test-out
  [& body]
  `(non-leaky-with-test-out ~@body))

(defmacro non-leaky-testing
  "Like clojure.test/testing, except body does not leak try/catch syntax."
  [string & body]
  `(ct/testing ~string
     (do ~@body)))

(defmacro testing
  [& args]
  `(non-leaky-testing ~@args))

;; TODO clojure.test/{with-test,deftest,deftest-,set-test,}
