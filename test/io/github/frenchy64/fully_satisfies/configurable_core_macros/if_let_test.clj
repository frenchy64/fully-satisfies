(ns io.github.frenchy64.fully-satisfies.configurable-core-macros.if-let-test
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.if-let :as if-let]))

(def my-if-let-opts {:rename {`if-let 'my-if-let}})

(if-let/->if-let `my-if-let-opts)

(deftest my-if-let-test
  (is (= 1 (my-if-let [x 1] x 2)))
  (is (= 2 (my-if-let [x nil] x 2))))

(def ^:dynamic *vol*)

(defmacro my-let [& args]
  `(do (vreset! *vol* true)
       (let ~@args)))

(def custom-let-opts {:rename {`if-let 'if-let-with-custom-let}
                      :replace {`let `my-let}})
(if-let/->if-let `custom-let-opts)

(deftest custom-let-test
  (let [vol (volatile! false)]
    (is (false? @vol))
    (binding [*vol* vol]
      (is (= 1 (if-let-with-custom-let [{:keys [x]} {:x 1}] x 2))))
    (is (true? @vol))))
