;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.everyp
  "An implementation of clojure.core/every-pred with a simple definitional equivalence."
  (:refer-clojure :exclude [every-pred]))

(defn everyp
  "Combines predicates into a variable-arity conjunction.
  
  Definitionally equivalent to:

    (defn everyp [& ps]
      (fn [& args] (every? #(every? % args) ps)))"
  {:arglists '([& ps])}
  ([] (fn
        ([] true)
        ([x] true)
        ([x y] true)
        ([x y z] true)
        ([x y z & args] true)))
  ([p1] (fn
          ([] true)
          ([x] (boolean (p1 x)))
          ([x y] (boolean (and (p1 x) (p1 y))))
          ([x y z] (boolean (and (p1 x) (p1 y) (p1 z))))
          ([x y z & args] (boolean (and (p1 x) (p1 y) (p1 z) (every? p1 args))))))
  ([p1 p2] (fn
             ([] true)
             ([x] (boolean (and (p1 x) (p2 x))))
             ([x y] (boolean (and (p1 x) (p1 y) (p2 x) (p2 y))))
             ([x y z] (boolean (and (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z))))
             ([x y z & args] (let [tp (fn [p] (and (p x) (p y) (p z) (every? p args)))]
                               (boolean (and (tp p1) (tp p2)))))))
  ([p1 p2 p3] (fn
                ([] true)
                ([x] (boolean (and (p1 x) (p2 x) (p3 x))))
                ([x y] (boolean (and (p1 x) (p1 y) (p2 x) (p2 y) (p3 x) (p3 y))))
                ([x y z] (boolean (and (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z) (p3 x) (p3 y) (p3 z))))
                ([x y z & args] (let [tp (fn [p] (and (p x) (p y) (p z) (every? p args)))]
                                  (boolean (and (tp p1) (tp p2) (tp p3)))))))
  ([p1 p2 p3 & ps] (fn
                     ([] true)
                     ([x] (boolean (and (p1 x) (p2 x) (p3 x) (every? (fn [p] (p x)) ps))))
                     ([x y] (boolean (and (p1 x) (p1 y) (p2 x) (p2 y) (p3 x) (p3 y) (every? (fn [p] (and (p x) (p y))) ps))))
                     ([x y z] (boolean (and (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z) (p3 x) (p3 y) (p3 z) (every? (fn [p] (and (p x) (p y) (p z))) ps))))
                     ([x y z & args] (let [tp (fn [p] (and (p x) (p y) (p z) (every? p args)))]
                                       (boolean (and (tp p1) (tp p2) (tp p3) (every? tp ps))))))))

(def every-pred everyp)
