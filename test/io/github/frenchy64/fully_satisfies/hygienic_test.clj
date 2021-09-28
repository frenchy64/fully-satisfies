(ns io.github.frenchy64.fully-satisfies.hygienic-test
  (:refer-clojure :exclude [locking binding with-bindings sync with-local-vars
                            with-in-str dosync with-precision with-loading-context
                            with-redefs delay])
  (:require [clojure.core :as cc]
            [clojure.test :refer [is are]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [io.github.frenchy64.fully-satisfies.hygienic :as hy]
            [clojure.template :as temp]))

(deftest locking-unhygienic-witnesses-and-fixes
  (let [Exception :Exception
        e :e
        catch (fn [ex e b]
                [ex e (inc b)])
        finally vector]
    (testing "locking"
      (testing "unhygienic"
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
      (testing "hygienic"
        (are [hygienic-locking] (= [:Exception :e 43]
                                   (hygienic-locking
                                     1
                                     (catch Exception e (inc 41))))
             hy/hygienic-locking
             hy/locking)
        (temp/do-template
          [hygienic-locking] (let [a (atom :init)]
                               (try (hygienic-locking
                                      1
                                      (throw (Exception.))
                                      (finally (reset! a :finally)))
                                    (catch Exception _))
                               (is (= :init @a)))
          hy/hygienic-locking
          hy/locking)))))

(def this-nsym (ns-name *ns*))

(deftest delay-unhygienic-witnesses-and-fixes
  (let [Exception :Exception
        e :e
        catch (fn [ex e b]
                [ex e (inc b)])
        finally vector]
    (testing "delay"
      (testing "unhygienic"
        (is (some? (eval `(cc/delay (recur))))))
      (testing "hygienic"
        (temp/do-template
          [hygienic-delay] (let [sym (symbol (ns-resolve this-nsym 'hygienic-delay))]
                             (testing sym
                               (try (eval (list sym
                                                '(recur)))
                                    (is false)
                                    (catch clojure.lang.Compiler$CompilerException e
                                      (is (= "Can only recur from tail position"
                                             (-> e .getCause .getMessage)))))))
          hy/hygienic-delay
          hy/delay)))))
