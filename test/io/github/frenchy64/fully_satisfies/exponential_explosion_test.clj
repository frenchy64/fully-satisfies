(ns io.github.frenchy64.fully-satisfies.exponential-explosion-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [io.github.frenchy64.fully-satisfies.exponential-explosion :as sut]))

(def this-ns (ns-name *ns*))

(defmacro duplicated [x] x)

(defmacro exponential [x]
  `(do ~x ~x))

(deftest double-expand-test
  (testing "pointer identity is used to compare args"
    (binding [sut/*state* (atom {})
              *ns* (the-ns this-ns)]
      ;;original form: (exponential (duplicated foo))
      (let [->foo #(symbol "foo")]
        (sut/lint-macro-call #'exponential [(list 'duplicated (->foo))])
        (sut/lint-macro-call #'duplicated [(->foo)])
        (sut/lint-macro-call #'duplicated [(->foo)]))
      (let [{:keys [history seen suspects] :as res} @sut/*state*]
        (is (not suspects) (pr-str res))
        res)))
  (testing "report if duplicate found after expanding parent"
    (binding [sut/*state* (atom {})
              *ns* (the-ns this-ns)]
      ;;original form: (exponential (duplicated foo))
      (let [foo 'foo]
        (sut/lint-macro-call #'exponential [(list 'duplicated foo)])
        (sut/lint-macro-call #'duplicated [foo])
        (sut/lint-macro-call #'duplicated [foo]))
      (let [{:keys [history seen suspects] :as res} @sut/*state*]
        (is suspects (pr-str res))
        (is (= ["Potentially exponential expansion detected:  (duplicated foo) "
                " [[#'io.github.frenchy64.fully-satisfies.exponential-explosion-test/exponential [(duplicated foo)]] [#'io.github.frenchy64.fully-satisfies.exponential-explosion-test/duplicated [foo]]]"
                ::TODO]
               (str/split-lines (with-out-str (sut/report-issues res)))))
        res)))
  (testing "ignore if duplicate found *before* expanding parent"
    (binding [sut/*state* (atom {})
              *ns* (the-ns this-ns)]
      ;;original form: (exponential (duplicated foo))
      (let [foo 'foo]
        (sut/lint-macro-call #'duplicated [foo])
        (sut/lint-macro-call #'exponential [(list 'duplicated foo)])
        (sut/lint-macro-call #'duplicated [foo]))
      (let [{:keys [history seen suspects] :as res} @sut/*state*]
        (is (not suspects) (pr-str res))
        res)))
  )
