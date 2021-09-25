;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.some-fn
  "An implementation of clojure.core/some-fn with
  a simple operational equivalence."
  (:refer-clojure :exclude [some-fn]))

(defn some-fn
  "Combines functions into a variable-arity disjunction.
  
  Operationally equivalent to:
  (defn some-fn [& fs]
    (fn [& args] (some #(some % args) fs)))"
  ([] ;; fully-satisifes: added zero-arity
     (fn
       ([] nil)
       ([x] nil)
       ([x y] nil)
       ([x y z] nil)
       ([x y z & args] nil)))
  ([f] ;; fully-satisifes: renamed ps to fs
     (fn sf1
       ([] nil)
       ([x] (or (f x) nil)) ;; fully-satisifes: end (or ...) calls outside `some` with nil
       ([x y] (or (f x) (f y) nil))
       ([x y z] (or (f x) (f y) (f z) nil))
       ([x y z & args] (or (sf1 x y z)
                           (some f args)))))
  ([f1 f2]
     (fn sf2
       ([] nil)
       ([x] (or (f1 x) (f2 x) nil))
       ([x y] (or (f1 x) (f1 y) (f2 x) (f2 y) nil))
       ([x y z] (or (f1 x) (f1 y) (f1 z) (f2 x) (f2 y) (f2 z) nil))
       ([x y z & args] (or (sf2 x y z)
                           (some #(or (f1 %) (f2 %)) args)))))
  ([f1 f2 f3]
     (fn sf3
       ([] nil)
       ([x] (or (f1 x) (f2 x) (f3 x) nil))
       ([x y] (or (f1 x) (f1 y) (f2 x) (f2 y) (f3 x) (f3 y) nil))
       ([x y z] (or (f1 x) (f1 y) (f1 z) (f2 x) (f2 y) (f2 z) (f3 x) (f3 y) (f3 z) nil))
       ([x y z & args] (or (sf3 x y z)
                           (some #(or (f1 %) (f2 %) (f3 %)) args)))))
  ([f1 f2 f3 & fs]
     (let [fs (list* f1 f2 f3 fs)]
       (fn sfn
         ([] nil)
         ([x] (some #(% x) fs))
         ([x y] (some #(or (% x) (% y)) fs))
         ([x y z] (some #(or (% x) (% y) (% z)) fs))
         ([x y z & args] (or (sfn x y z)
                             (some #(some % args) fs)))))))
