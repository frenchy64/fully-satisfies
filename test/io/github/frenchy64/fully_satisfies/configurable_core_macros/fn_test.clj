(ns io.github.frenchy64.fully-satisfies.configurable-core-macros.fn-test
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.fn :as fn]))

(def my-fn-opts {:rename {`fn 'my-fn}})

(fn/->fn `my-fn-opts)

(deftest my-fn-test
  (is (= 1 ((my-fn [x] x) 1))))

(def ^:dynamic *vol*)

(defmacro my-let [& args]
  `(do (vreset! *vol* true)
       (let ~@args)))

(def custom-let-opts {:rename {`fn 'fn-with-custom-let}
                      :replace {`let `my-let}})
(fn/->fn `custom-let-opts)

(deftest custom-let-test
  (let [vol (volatile! false)]
    (is (false? @vol))
    (binding [*vol* vol]
      (is (= 1 ((fn-with-custom-let [{:keys [x]}] x) {:x 1}))))
    (is (true? @vol))))
