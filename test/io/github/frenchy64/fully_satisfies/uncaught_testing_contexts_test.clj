(ns io.github.frenchy64.fully-satisfies.uncaught-testing-contexts-test
  (:require [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [clojure.test :as t :refer [test-var test-vars is report *testing-contexts*]]))

(def test-var-tests-prefix "test-var-exception-test-")

(defmacro deftests-for-test-var
  "Define unit tests for clojure.test/test-var functionality. See
  `test-ns-hook` in this namespace for special handling.
  
  Each test should throw an exception with {::expected true} ex-data,
  and the expected :error report :message should be attached via the ::expected-test-var-message
  key of the test name's metadata."
  [& ts]
  {:pre [(even? (count ts))]}
  `(do ~@(map (fn [[nme & body]]
                {:pre [(simple-symbol? nme)]}
                `(deftest ~(with-meta (symbol (str test-var-tests-prefix (name nme)))
                                      (meta nme))
                   ~@body))
              (partition 2 ts))))

(def most-likely-msg-prefix "Uncaught exception, possibly thrown in testing context: ")

(deftests-for-test-var
  ^{::expected-test-var-message "Uncaught exception, not in assertion."}
  thrown-from-empty-context-test
  (throw (ex-info "" {::expected true}))

  ^{::expected-test-var-message "Uncaught exception, not in assertion."}
  rethrown-between-empty-contexts-test
  (let [e (try (binding [*testing-contexts* (list)]
                 (throw (ex-info "" {::expected true})))
               (catch Exception e e))]
   (binding [*testing-contexts* (list)]
     (throw e)))

  ^{::expected-test-var-message (str most-likely-msg-prefix "foo bar")}
  nested-exception-test
  (testing "foo"
    (testing "bar"
      (throw (ex-info "" {::expected true}))))

  ^{::expected-test-var-message (str most-likely-msg-prefix "foo bar 1 2 3 4 5")}
  long-nested-exception-test
  (testing "foo"
    (testing "bar"
      (testing "1"
        (testing "2"
          (testing "3"
            (testing "4"
              (testing "5"
                (throw (ex-info "" {::expected true})))))))))

  ^{::expected-test-var-message (str most-likely-msg-prefix "foo bar")}
  rethrown-nested-exception-test
  (let [the-e (atom nil)]
    (testing "foo"
      (try (testing "bar"
             (throw (reset! the-e (ex-info "" {::expected true}))))
           (catch Exception e
             (assert (identical? e @the-e))
             (throw e)))))

  ;; this is a weird case
  ^{::expected-test-var-message (str most-likely-msg-prefix "foo bar")}
  rethrown-from-empty-context-test
  (let [the-e (atom nil)]
    (try 
      (testing "foo"
        (testing "bar"
          (throw (reset! the-e (ex-info "" {::expected true})))))
      (catch Exception e
        (assert (identical? @the-e e))
        (throw e))))

  ^{::expected-test-var-message (str most-likely-msg-prefix "adjacent")}
  rethrown-from-adjacent-context
  (let [the-e (atom nil)
        e (testing "foo"
            (try (testing "bar"
                   (throw (reset! the-e (ex-info "" {::expected true}))))
                 (catch Exception e
                   (assert (identical? @the-e e))
                   e)))]
    (assert (identical? @the-e e))
    (testing "adjacent"
      (throw e)))

  ^{::expected-test-var-message (str most-likely-msg-prefix "adjacent")}
  thrown-in-non-nested-context-rethrown-from-adjacent-context
  (let [the-e (atom nil)
        e (testing "foo"
            (try (throw (reset! the-e (ex-info "" {::expected true})))
                 (catch Exception e
                   (assert (identical? @the-e e))
                   e)))]
   (testing "adjacent"
     (assert (identical? @the-e e))
     (throw e)))

  ^{::expected-test-var-message (str most-likely-msg-prefix "foo bar")}
  implicit-binding-conveyance-test
  (testing "foo"
    (testing "bar"
      @(future
         (testing "baz"
           (throw (ex-info "" {::expected true}))))))

  ^{::expected-test-var-message (str most-likely-msg-prefix "foo bar")}
  explicit-binding-conveyance-test
  (testing "foo"
    (testing "bar"
      (let [f (bound-fn []
                (testing "baz"
                  (throw (ex-info "" {::expected true}))))]
        @(future (f))))))

(deftest testing-try-catch-finally-leak-test
  (let [finally vector
        catch vector
        Exception ::Exception
        e ::e]
    (is (= [::Exception ::e ::e]
           (testing "anything"
             (catch Exception e e))))
    (is (= [::foo ::bar]
           (testing "anything"
             (finally ::foo ::bar))))))

(def this-ns-name (ns-name *ns*))

;; test-ns-hook will be used by test/test-ns to run tests in this
;; namespace.
(defn test-ns-hook []
  (let [{test-var-test-vars true
         other-test-vars false
         :as all-groups} (group-by #(-> % symbol name (.startsWith test-var-tests-prefix))
                                   (sort-by symbol (vals (ns-interns this-ns-name))))]
    ;; extra paranoid checks of group-by usage
    (assert (= 2 (count all-groups)) (count all-groups))
    (assert (seq test-var-test-vars))
    (assert (seq other-test-vars))
    (test-vars other-test-vars)
    ;; testing clojure.test/test-var
    (doseq [v test-var-test-vars]
      ;; don't wrap in `testing` until _after_ test-var call
      (let [rs (atom [])
            actual (into []
                         (remove (comp #{:begin-test-var :end-test-var} :type))
                         (binding [report #(swap! rs conj %)]
                           (t/test-var v)))
            expected [{:type :error :message (-> v meta ::expected-test-var-message)}]]
        (testing (str `test-ns-hook "\n" (symbol v))
          ;; find ex-info
          (let [e (when (-> actual first :type #{:error})
                    (-> actual first :actual))]
            (is (::expected
                  (some 
                    ex-data
                    (take-while some? (iterate #(some-> ^Exception % .getCause)
                                               e))))
                e))
          (is (= expected
                 (map #(select-keys % [:type :message]) actual))))))))
