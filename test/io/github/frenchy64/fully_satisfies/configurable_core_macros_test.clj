(ns io.github.frenchy64.fully-satisfies.configurable-core-macros-test
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros :as configurable-core-macros]))

(def opts {:rename {`let `my-let
                    `fn `my-fn
                    `defn `my-defn}})

(configurable-core-macros/->clojure-core `opts)

;;FIXME
(my-defn f [&form])
(my-defn g ([&form &env]) ([&form &env arg]))
(my-defn my-identity [x] x)

(deftest ->fn-test
  (is (= 1 (my-identity 1))))

(deftest ->defn-test
  (testing "fixes Clojure bug"
    (is (= '([&form]) (-> #'f meta :arglists)))
    (is (= '([&form &env] [&form &env arg]) (-> #'g meta :arglists))))
  (is (= 1 (my-identity 1))))
