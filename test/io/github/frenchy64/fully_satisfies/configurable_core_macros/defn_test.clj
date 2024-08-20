(ns io.github.frenchy64.fully-satisfies.configurable-core-macros.defn-test
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.defn :as defn]))

(def opts {:rename {`defn `my-defn}})

(defn/->defn `opts)

(my-defn f [&form])
(my-defn g ([&form &env]) ([&form &env arg]))
(my-defn my-identity [x] x)

(deftest ->defn-test
  (testing "fixes Clojure bug"
    (is (= '([&form]) (-> #'f meta :arglists)))
    (is (= '([&form &env] [&form &env arg]) (-> #'g meta :arglists))))
  (is (= 1 (my-identity 1))))
