(ns io.github.frenchy64.fully-satisfies.defmulti-cache-test
  (:refer-clojure :exclude [defmulti])
  (:require [clojure.test :refer [is are]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts
             :refer [deftest testing]]
            [io.github.frenchy64.fully-satisfies.defmulti-cache :refer [defmulti]]))

(defmulti foo identity)
(defmethod foo :bar [_] 'bar)
(defmethod foo :baz [_] 'baz)
(defmethod foo :default [_] 'default)

(deftest basic-semantics
  (is (= 'bar (foo :bar)))
  (is (= 'baz (foo :baz)))
  (is (= 'default (foo (gensym)))))
