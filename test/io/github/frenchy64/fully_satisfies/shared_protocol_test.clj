(ns io.github.frenchy64.fully-satisfies.shared-protocol-test
  (:refer-clojure :exclude [defprotocol])
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [testing deftest]]
            [io.github.frenchy64.fully-satisfies.shared-protocol :refer [def-shared-protocol defprotocol]]))

(defprotocol IntermediateProtShared
  (intermediate-method-shared [this]))

(def intermediate-method-shared1 intermediate-method-shared)

(extend-protocol IntermediateProtShared
  String
  (intermediate-method-shared [_] :ok))

(def intermediate-method-shared2 intermediate-method-shared)

(defprotocol IntermediateProt
  (intermediate-method [this]))

(def intermediate-method1 intermediate-method)

(extend-protocol IntermediateProt
  String
  (intermediate-method [_] :ok))

(def intermediate-method2 intermediate-method)

(deftest test-protocol-can-see-future-extensions
  (let [v "foo"]
    (is (= intermediate-method
           intermediate-method1
           intermediate-method2))
    (is (= :ok
           (intermediate-method v)
           (intermediate-method1 v)
           (intermediate-method2 v))))
  (let [v "foo"]
    (is (= intermediate-method-shared
           intermediate-method-shared1
           intermediate-method-shared2))
    (is (= :ok
           (intermediate-method-shared v)
           (intermediate-method-shared1 v)
           (intermediate-method-shared2 v)))))

