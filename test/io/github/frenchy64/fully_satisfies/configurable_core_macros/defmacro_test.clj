(ns io.github.frenchy64.fully-satisfies.configurable-core-macros.defmacro-test
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.defmacro :as defmacro]))

(def opts {:rename {`defmacro `my-defmacro}})

(defmacro/->defmacro `opts)

(my-defmacro f [a b] [a b])

(deftest ->defmacro-test
  (is (= '([a b]) (-> #'f meta :arglists)))
  (is (= [2 3] (f (inc 1) (inc 2))))
  (is (= '[(clojure.core/inc 1) (clojure.core/inc 2)] (macroexpand-1 `(f (inc 1) (inc 2))))))
