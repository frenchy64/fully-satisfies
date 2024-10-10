;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.clearing-future
  "futures that clear conveyed bindings after execution.

  Fixes memory leak described in https://clojure.atlassian.net/browse/CLJ-2619"
  (:refer-clojure :exclude [future future-call]))

(set! *warn-on-reflection* true)

(defn binding-conveyor-fn
  {:private true
   :added "1.3"}
  [f]
  (let [frame (clojure.lang.Var/cloneThreadBindingFrame)]
    (fn 
      ([]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f))
      ([x]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f x))
      ([x y]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f x y))
      ([x y z]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f x y z))
      ([x y z & args] 
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (apply f x y z args)))))

(defn ^:private deref-clearing-future
  ([^java.util.concurrent.Future fut]
     (.get fut))
  ([^java.util.concurrent.Future fut timeout-ms timeout-val]
     (try (.get fut timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
          (catch java.util.concurrent.TimeoutException e
            timeout-val))))

(defn clearing-future-call
  "Takes a function of no args and yields a future object that will
  invoke the function in another thread, and will cache the result and
  return it on all subsequent calls to deref/@. Clears conveyed bindings
  after result has been computed. If the computation has
  not yet finished, calls to deref/@ will block, unless the variant
  of deref with timeout is used. See also - realized?."
  [f]
  (let [f (binding-conveyor-fn f)
        fut (.submit clojure.lang.Agent/soloExecutor
                     ^Callable
                     #(let [o (clojure.lang.Var/getThreadBindingFrame)]
                        (try (f)
                             (finally (clojure.lang.Var/resetThreadBindingFrame o)))))]
    (reify 
     clojure.lang.IDeref 
     (deref [_] (deref-clearing-future fut))
     clojure.lang.IBlockingDeref
     (deref
      [_ timeout-ms timeout-val]
      (deref-clearing-future fut timeout-ms timeout-val))
     clojure.lang.IPending
     (isRealized [_] (.isDone fut))
    java.util.concurrent.Future
      (get [_] (.get fut))
      (get [_ timeout unit] (.get fut timeout unit))
      (isCancelled [_] (.isCancelled fut))
      (isDone [_] (.isDone fut))
      (cancel [_ interrupt?] (.cancel fut interrupt?)))))
  
(defmacro clearing-future
  "Takes a body of expressions and yields a future object that will
  invoke the body in another thread, and will cache the result and
  return it on all subsequent calls to deref/@. Clears conveyed bindings
  after result has been computed. If the computation has
  not yet finished, calls to deref/@ will block, unless the variant of
  deref with timeout is used. See also - realized?."
  [& body] `(clearing-future-call (^{:once true} fn* [] ~@body))) 

(defn future-call [f] (clearing-future-call f))
(defmacro future [& body] `(clearing-future ~@body))
