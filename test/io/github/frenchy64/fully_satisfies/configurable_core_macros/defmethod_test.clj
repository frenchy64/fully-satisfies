(ns io.github.frenchy64.fully-satisfies.configurable-core-macros.defmethod-test
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.defmethod :as defmethod]))

(def opts {:rename {`defmethod `my-defmethod}})

(defmethod/->defmethod `opts)

(defmulti foo :op)
(my-defmethod foo :foo [a] (:foo a))
(my-defmethod foo :bar [a] (:bar a))
(my-defmethod foo :default [a] ::default)

(deftest ->defmethod-test
  (is (= 1 (foo {:op :foo :foo 1})))
  (is (= ::bar (foo {:op :bar :bar ::bar})))
  (is (= ::default (foo nil))))
