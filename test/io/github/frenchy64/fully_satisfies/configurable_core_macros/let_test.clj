(ns io.github.frenchy64.fully-satisfies.configurable-core-macros.let-test
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.let :as let]))

(def my-let-opts {:rename {`let `my-let}})

(let/->let `my-let-opts)

(deftest my-let-test
  (is (= 1 (my-let [x 1] x)))
  (is (= {:pre []} (my-let [x 1] {:pre []})))
  (is (= 4 (my-let [{:keys [a]} {:a 4}] a))))

(def ^:dynamic *vol* nil)

(defn my-destructure [bindings]
  (destructure
    (into [(gensym '_) `(vreset! *vol* true)]
          bindings)))

(def custom-let-opts {:rename {`let `let-with-custom-destructure}
                      :replace {`destructure `my-destructure}})
(let/->let `custom-let-opts)

(deftest custom-let-test
  (let [vol (volatile! false)]
    (is (false? @vol))
    (binding [*vol* vol]
      (is (= 1 (let-with-custom-destructure [{:keys [x]} {:x 1}] x))))
    (is (true? @vol))))
