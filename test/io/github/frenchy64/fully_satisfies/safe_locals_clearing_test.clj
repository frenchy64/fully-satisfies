(ns io.github.frenchy64.fully-satisfies.safe-locals-clearing-test
  (:refer-clojure :exclude [delay lazy-seq])
  (:require [clojure.core :as cc]
            [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [io.github.frenchy64.fully-satisfies.safe-locals-clearing :as safe]))

(def ^:dynamic *recursive* false)
(def ^:dynamic *atom* (atom {}))

(deftest broken-test
  (testing "f is cleared on recursive call"
    (let [f +
          p (promise)
          d (cc/delay (f) @@p)]
      (deliver p d)
      (is (thrown? NullPointerException @d))))
  (testing "doesn't seem to clear local here, maybe because n is a long"
    (binding [*atom* (atom [])]
      (let [n 1
            self (promise)
            d (cc/delay
                (swap! *atom* conj n)
                (when-not *recursive*
                  (binding [*recursive* true]
                    @@self))
                n)]
        (deliver self d)
        (is (= 1 @d))
        (is (= [1 1] @*atom*)))))
  (testing "doesn't seem to clear local here, maybe because n is a long"
    (binding [*atom* (atom [])]
      (let [n identity
            self (promise)
            d (cc/delay
                (swap! *atom* conj n)
                (when-not *recursive*
                  (binding [*recursive* true]
                    @@self))
                n)]
        (deliver self d)
        (is (= nil @d))
        (is (= [n n] @*atom*)))))
  (testing "clears f, seemingly because we invoked it"
    (binding [*atom* (atom [])]
      (let [f #(do nil)
            self (promise)
            d (cc/delay
                (swap! *atom* conj f)
                (f)
                (when-not *recursive*
                  (binding [*recursive* true]
                    @@self)))]
        (deliver self d)
        (is (thrown? NullPointerException @d))
        (is (= [f nil] @*atom*))))))

(deftest safe-test
  (is (cc/delay? (safe/delay)))
  (is (= 1 (cc/force (safe/delay 1))))
  (testing "f is cleared on recursive call"
    (let [f +
          p (promise)
          d (safe/delay (f) @@p)]
      (deliver p d)
      (is (thrown-with-msg? Exception #"Recursive delay dereference" @d))))
  (testing "doesn't seem to clear local here, maybe because n is a long"
    (binding [*atom* (atom [])]
      (let [n 1
            self (promise)
            d (safe/delay
                (swap! *atom* conj n)
                (when-not *recursive*
                  (binding [*recursive* true]
                    @@self))
                n)]
        (deliver self d)
        (is (thrown-with-msg? Exception #"Recursive delay dereference" @d))
        (is (= [1] @*atom*)))))
  (testing "doesn't seem to clear local here, maybe because n is a long"
    (binding [*atom* (atom [])]
      (let [n identity
            self (promise)
            d (safe/delay
                (swap! *atom* conj n)
                (when-not *recursive*
                  (binding [*recursive* true]
                    @@self))
                n)]
        (deliver self d)
        (is (thrown-with-msg? Exception #"Recursive delay dereference" @d))
        (is (= [n] @*atom*)))))
  (testing "clears f, seemingly because we invoked it"
    (binding [*atom* (atom [])]
      (let [f #(do nil)
            self (promise)
            d (safe/delay
                (swap! *atom* conj f)
                (f)
                (when-not *recursive*
                  (binding [*recursive* true]
                    @@self)))]
        (deliver self d)
        (is (thrown-with-msg? Exception #"Recursive delay dereference" @d))
        (is (= [f] @*atom*))))))
