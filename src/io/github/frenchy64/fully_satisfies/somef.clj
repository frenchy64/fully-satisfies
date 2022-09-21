;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.somef
  "An implementation of clojure.core/some-fn with a simple definitional equivalence."
  (:refer-clojure :exclude [some-fn]))

(defn somef
  "Combines functions into a variable-arity disjunction.
  
  Definitionally equivalent to:

    (defn somef [& fs]
      (fn [& args] (some #(some % args) fs)))"
  {:arglists '([& fs])}
  ([] (fn ([] nil) ([x] nil) ([x y] nil) ([x y z] nil) ([x y z & args] nil)))
  ([f1] (fn
          ([] nil)
          ([x] (or (f1 x) nil))
          ([x y] (or (f1 x) (f1 y) nil))
          ([x y z] (or (f1 x) (f1 y) (f1 z) nil))
          ([x y z & args] (or (f1 x) (f1 y) (f1 z) (some f1 args)))))
  ([f1 f2] (fn
             ([] nil)
             ([x] (or (f1 x) (f2 x) nil))
             ([x y] (or (f1 x) (f1 y)
                        (f2 x) (f2 y)
                        nil))
             ([x y z] (or (f1 x) (f1 y) (f1 z)
                          (f2 x) (f2 y) (f2 z)
                          nil))
             ([x y z & args] (let [tf (fn [f] (or (f x) (f y) (f z) (some f args)))]
                               (or (tf f1) (tf f2))))))
  ([f1 f2 f3] (fn
                ([] nil)
                ([x] (or (f1 x) (f2 x) (f3 x) nil))
                ([x y] (or (f1 x) (f1 y)
                           (f2 x) (f2 y)
                           (f3 x) (f3 y)
                           nil))
                ([x y z] (or (f1 x) (f1 y) (f1 z)
                             (f2 x) (f2 y) (f2 z)
                             (f3 x) (f3 y) (f3 z)
                             nil))
                ([x y z & args] (let [tf (fn [f] (or (f x) (f y) (f z) (some f args)))]
                                  (or (tf f1) (tf f2) (tf f3))))))
  ([f1 f2 f3 & fs] (fn
                     ([] nil)
                     ([x] (or (f1 x) (f2 x) (f3 x) (some (fn [f] (f x)) fs)))
                     ([x y] (or (f1 x) (f1 y)
                                (f2 x) (f2 y)
                                (f3 x) (f3 y)
                                (some (fn [f] (or (f x) (f y))) fs)))
                     ([x y z] (or (f1 x) (f1 y) (f1 z)
                                  (f2 x) (f2 y) (f2 z)
                                  (f3 x) (f3 y) (f3 z)
                                  (some (fn [f] (or (f x) (f y) (f z))) fs)))
                     ([x y z & args] (let [tf (fn [f] (or (f x) (f y) (f z) (some f args)))]
                                       (or (tf f1) (tf f2) (tf f3) (some tf fs)))))))

(def some-fn somef)
