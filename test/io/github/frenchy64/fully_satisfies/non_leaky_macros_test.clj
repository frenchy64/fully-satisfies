;; TODO move to non-leaky-macros.clojure.core-test
(ns io.github.frenchy64.fully-satisfies.non-leaky-macros-test
  (:refer-clojure :exclude [locking binding with-bindings sync with-local-vars
                            with-in-str dosync with-precision with-loading-context
                            with-redefs delay])
  (:require [clojure.core :as cc]
            [clojure.test :refer [is are]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [io.github.frenchy64.fully-satisfies.non-leaky-macros.clojure.core :as nl-cc]
            [clojure.template :as temp]))

(deftest locking-leak-witnesses-and-fixes
  (let [Exception :Exception
        e :e
        catch (fn [ex e b]
                [ex e (inc b)])
        finally vector]
    (testing "locking"
      (testing "leaks"
        (is (nil?
              (cc/locking
                1
                (catch Exception e (inc 41)))))
        (try (eval `(cc/locking
                      1
                      (throw (Exception.))
                      (finally)))
             (is false)
             (catch RuntimeException e
               (is (= "finally clause must be last in try expression"
                      (-> e .getCause .getMessage))))))
      (testing "plugged leaks"
        (are [non-leaky-locking] (= [:Exception :e 43]
                                    (non-leaky-locking
                                      1
                                      (catch Exception e (inc 41))))
             nl-cc/non-leaky-locking
             nl-cc/locking)
        (temp/do-template
          [non-leaky-locking] (let [a (atom :init)]
                                (try (non-leaky-locking
                                       1
                                       (throw (Exception.))
                                       (finally (reset! a :finally)))
                                     (catch Exception _))
                                (is (= :init @a)))
          nl-cc/non-leaky-locking
          nl-cc/locking)))))

(def this-nsym (ns-name *ns*))

(deftest delay-leak-witnesses-and-fixes
  (let [Exception :Exception
        e :e
        catch (fn [ex e b]
                [ex e (inc b)])
        finally vector]
    (testing "delay"
      (testing "leaks"
        (is (some? (eval `(cc/delay (recur))))))
      (testing "plugged leaks"
        (temp/do-template
          [non-leaky-delay] (let [sym (symbol (ns-resolve this-nsym 'non-leaky-delay))]
                              (testing sym
                                (try (eval (list sym
                                                 '(recur)))
                                     (is false)
                                     (catch clojure.lang.Compiler$CompilerException e
                                       (is (= "Can only recur from tail position"
                                              (-> e .getCause .getMessage)))))))
          nl-cc/non-leaky-delay
          nl-cc/delay)))))
