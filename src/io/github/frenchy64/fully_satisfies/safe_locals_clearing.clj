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
  instead now throwing an exception instead of risking executing 
  locals-cleared garbage.

  This article explains how locals clearing enables tail calls
  to potentially have strong references to their arguments.
  https://clojure.org/reference/lazy#_dont_hang_onto_your_head
  
  Fixes https://clojure.atlassian.net/browse/CLJ-2861
  
  Approach: add the following before acquiring a lock.

    if(l.isHeldByCurrentThread()) {
      throw Util.sneakyThrow(Util.runtimeException(\"Recursive delay dereference\"));
    }"
  (:refer-clojure :exclude [delay lazy-seq])
  (:require [clojure.core :as cc])
  (:import [io.github.frenchy64.fully_satisfies.safe_locals_clearing Delay LazySeq]))

(defmacro delay
  "Takes a body of expressions and yields a Delay object that will
  invoke the body only the first time it is forced (with force or deref/@), and
  will cache the result and return it on all subsequent force
  calls. See also - realized?
  
  Throws if dereferenced recursively and hides the recur target from body."
  {:added "1.0"}
  [& body]
  `(io.github.frenchy64.fully_satisfies.safe_locals_clearing.Delay.
     (^{:once true} fn* [] (let* [res# (do ~@body)] res#))))

;;TODO unit test
(defmacro lazy-seq
  "Takes a body of expressions that returns an ISeq or nil, and yields
  a Seqable object that will invoke the body only the first time seq
  is called, and will cache the result and return it on all subsequent
  seq calls. See also - realized?
  
  Throws if dereferenced recursively."
  {:added "1.0"}
  [& body]
  (list 'new 'io.github.frenchy64.fully_satisfies.safe_locals_clearing.LazySeq (list* '^{:once true} fn* [] body)))    
