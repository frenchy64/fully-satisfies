;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.future
  "futures that clear conveyed bindings after execution.

  Fixes memory leak described in https://clojure.atlassian.net/browse/CLJ-2619"
  (:refer-clojure :exclude [future future-call]))

(set! *warn-on-reflection* true)

(defn ^:private future'-binding-conveyor-fn
  [f]
  (let [frame (clojure.lang.Var/cloneThreadBindingFrame)]
    (fn
      ([]
       (let [previous-frame (clojure.lang.Var/getThreadBindingFrame)]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (try
           (f)
           (finally
             (clojure.lang.Var/resetThreadBindingFrame previous-frame)))))
      ([x]
       (let [previous-frame (clojure.lang.Var/getThreadBindingFrame)]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (try
           (f x)
           (finally
             (clojure.lang.Var/resetThreadBindingFrame previous-frame)))))
      ([x y]
       (let [previous-frame (clojure.lang.Var/getThreadBindingFrame)]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (try
           (f x y)
           (finally
             (clojure.lang.Var/resetThreadBindingFrame previous-frame)))))
      ([x y z]
       (let [previous-frame (clojure.lang.Var/getThreadBindingFrame)]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (try
           (f x y z)
           (finally
             (clojure.lang.Var/resetThreadBindingFrame previous-frame)))))
      ([x y z & args] 
       (let [previous-frame (clojure.lang.Var/getThreadBindingFrame)]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (try
           (apply f x y z args)
           (finally
             (clojure.lang.Var/resetThreadBindingFrame previous-frame))))))))

(defn ^:private deref-future'
  ([^java.util.concurrent.Future fut]
     (.get fut))
  ([^java.util.concurrent.Future fut timeout-ms timeout-val]
     (try (.get fut timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
          (catch java.util.concurrent.TimeoutException e
            timeout-val))))

(defn future-call'
  "Takes a function of no args and yields a future object that will
  invoke the function in another thread, and will cache the result and
  return it on all subsequent calls to deref/@. If the computation has
  not yet finished, calls to deref/@ will block, unless the variant
  of deref with timeout is used. See also - realized?."
  [f]
  (let [f (future'-binding-conveyor-fn f)
        fut (.submit clojure.lang.Agent/soloExecutor ^Callable f)]
    (reify 
     clojure.lang.IDeref 
     (deref [_] (deref-future' fut))
     clojure.lang.IBlockingDeref
     (deref
      [_ timeout-ms timeout-val]
      (deref-future' fut timeout-ms timeout-val))
     clojure.lang.IPending
     (isRealized [_] (.isDone fut))
    java.util.concurrent.Future
      (get [_] (.get fut))
      (get [_ timeout unit] (.get fut timeout unit))
      (isCancelled [_] (.isCancelled fut))
      (isDone [_] (.isDone fut))
      (cancel [_ interrupt?] (.cancel fut interrupt?)))))
  
(defmacro future'
  "Takes a body of expressions and yields a future object that will
  invoke the body in another thread, and will cache the result and
  return it on all subsequent calls to deref/@. If the computation has
  not yet finished, calls to deref/@ will block, unless the variant of
  deref with timeout is used. See also - realized?."
  [& body] `(future-call' (^{:once true} fn* [] ~@body))) 
