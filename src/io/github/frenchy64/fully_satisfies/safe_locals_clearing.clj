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
  to potentially to inherit strong references to their arguments
  from closed over locals.
  https://clojure.org/reference/lazy#_dont_hang_onto_your_head
  
  Fixes https://clojure.atlassian.net/browse/CLJ-2861

  There are two ways a ^:once function can be called in a delay or lazy-seq:
  1. the thread that acquires the lock to call the body then realizes
     the same object by calling the body.
  2. the body has a (recur) call in tail position.
  
  The first issue can be handled by first checking if the realize lock is already
  held by the current thread.

    if(l.isHeldByCurrentThread()) {
      throw Util.sneakyThrow(Util.runtimeException(\"Recursive delay dereference\"));
    }

  We do not do this because we don't have access to the locks of Delay and LazySeq.
  
  The second issue could be better supported by the Clojure compiler.
  The form (^:once fn [] (recur)) probably should throw a compile-time error.

  Instead, we take advantage of locals clearing to detect recursive calls:

    (let [x true]
      (^:once fn* [] (assert x) (recur)))

  This has a runtime cost. An alternative could be move the body out of tail position,
  which would result in a compile-time error:

    (^:once fn* [] (let [x (recur)] x))
  
  Here we use the assertion approach to handle all cases for portability
  and compatibility."
  (:refer-clojure :exclude [delay lazy-seq]))

(defmacro delay
  "Takes a body of expressions and yields a Delay object that will
  invoke the body only the first time it is forced (with force or deref/@), and
  will cache the result and return it on all subsequent force
  calls. See also - realized?
  
  Throws if dereferenced recursively. Explicitly disallows :pre/:post map."
  {:added "1.0"}
  [& body]
  `(clojure.lang.Delay.
     (let* [x# true]
       (^:once fn* [] (when-not x# (throw (Exception. ~(str "Recursive delay detected: " (pr-str &form))))) ~@body))))

;;TODO unit test
(defmacro lazy-seq
  "Takes a body of expressions that returns an ISeq or nil, and yields
  a Seqable object that will invoke the body only the first time seq
  is called, and will cache the result and return it on all subsequent
  seq calls. See also - realized?
  
  Throws if realized recursively. Explicitly disallows :pre/:post map."
  {:added "1.0"}
  [& body]
  `(clojure.lang.LazySeq.
     (let* [x# true]
       (^:once fn* [] (when-not x# (throw (Exception. ~(str "Recursive lazy-seq detected: " (pr-str &form))))) ~@body))))
