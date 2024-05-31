;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.safe-locals-clearing
  "Provides safer versions of delay and lazy-seq that ensure the
  function calculating their cached values is only called once
  while preserving their locals-clearing capabilities.

  The tradeoff being they now cannot be recursively dereferenced,
  instead now throwing an exception instead of risking executing garbage.
  
  Fixes https://clojure.atlassian.net/browse/CLJ-2861
  
  Approach: add the following before acquiring a lock.

    if(l.isHeldByCurrentThread()) {
      throw Util.sneakyThrow(Util.runtimeException(\"Recursive delay dereference\"));
    }"
  (:refer-clojure :exclude [delay lazy-seq])
  (:import io.github.frenchy64.fully_satisfies.safe_locals_clearing.Delay))

(defmacro delay
  "Takes a body of expressions and yields a Delay object that will
  invoke the body only the first time it is forced (with force or deref/@), and
  will cache the result and return it on all subsequent force
  calls. See also - realized?
  
  Throws if dereferenced recursively."
  {:added "1.0"}
  [& body]
    (list 'new 'io.github.frenchy64.fully_satisfies.safe_locals_clearing.Delay (list* `^{:once true} fn* [] body)))

(defn delay?
  "returns true if x is a Delay created with delay"
  {:added "1.0"
   :static true}
  [x] (instance? io.github.frenchy64.fully_satisfies.safe_locals_clearing.Delay x))

(defn force
  "If x is a Delay, returns the (possibly cached) value of its expression, else returns x"
  {:added "1.0"
   :static true}
  [x] (. io.github.frenchy64.fully_satisfies.safe_locals_clearing.Delay (force x)))
