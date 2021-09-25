(ns io.github.frenchy64.fully-satisfies.never-test
  (:require [clojure.test :refer :all]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [clojure.test.check.generators :as gen]
            [io.github.frenchy64.fully-satisfies.never :refer [never?]]))

(deftest never?-test
  (checking "never? returns false"
    [x gen/any]
    (is (false? (never? x)))))
