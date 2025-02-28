(ns io.github.frenchy64.fully-satisfies.exponential-explosion-test
  (:require [clojure.test :refer [deftest is testing]]
            [io.github.frenchy64.fully-satisfies.exponential-explosion :as sut]))

(def this-ns (ns-name *ns*))

(defmacro duplicated [x] x)

(defmacro exponential [x]
  `(do ~x ~x))

(deftest double-expand-test
  (binding [sut/*state* (atom {})
            *ns* (the-ns this-ns)]
    ;;original form: (exponential (duplicated foo))
    (let [foo 'foo]
      (sut/lint-macro-call #'exponential [(list 'duplicated foo)])
      (sut/lint-macro-call #'duplicated [foo])
      (sut/lint-macro-call #'duplicated [foo]))
    (let [{:keys [history seen suspects] :as res} @sut/*state*]
      (is suspects (pr-str res))
      res))
  )
