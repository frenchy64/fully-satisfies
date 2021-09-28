(ns io.github.frenchy64.fully-satisfies.latest-protocol-test
  (:refer-clojure :exclude [satisfies? find-protocol-impl find-protocol-method extends?])
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [testing deftest]]
            [io.github.frenchy64.fully-satisfies.latest-protocol :as lp]))

(defprotocol P
  (foo [this]))

(def P1 P)

(defrecord A [])

(extend-protocol P
  A
  (foo [this] :A))

(def P2 P)

(deftest latest-protocol-test
  (is (lp/satisfies-latest? P (->A)))
  (is (lp/satisfies-latest? P1 (->A)))
  (is (lp/satisfies-latest? P2 (->A)))
  (is (lp/satisfies? P (->A)))
  (is (lp/satisfies? P1 (->A)))
  (is (lp/satisfies? P2 (->A)))
  (is (lp/extends-latest? P A))
  (is (lp/extends-latest? P1 A))
  (is (lp/extends-latest? P2 A))
  (is (lp/extends? P A))
  (is (lp/extends? P1 A))
  (is (lp/extends? P2 A))
  (is (= [A] (lp/latest-extenders P)))
  (is (= [A] (lp/latest-extenders P1)))
  (is (= [A] (lp/latest-extenders P2)))
  (is (= [A] (lp/extenders P)))
  (is (= [A] (lp/extenders P1)))
  (is (= [A] (lp/extenders P2)))
  (is (= :A ((:foo (lp/find-latest-protocol-impl P (->A))) (->A))))
  (is (= :A ((:foo (lp/find-latest-protocol-impl P1 (->A))) (->A))))
  (is (= :A ((:foo (lp/find-latest-protocol-impl P2 (->A))) (->A))))
  (is (= :A ((:foo (lp/find-protocol-impl P (->A))) (->A))))
  (is (= :A ((:foo (lp/find-protocol-impl P1 (->A))) (->A))))
  (is (= :A ((:foo (lp/find-protocol-impl P2 (->A))) (->A))))
  (is (= :A ((lp/find-latest-protocol-method P :foo (->A)) (->A))))
  (is (= :A ((lp/find-latest-protocol-method P1 :foo (->A)) (->A))))
  (is (= :A ((lp/find-latest-protocol-method P2 :foo (->A)) (->A))))
  (is (= :A ((lp/find-protocol-method P :foo (->A)) (->A))))
  (is (= :A ((lp/find-protocol-method P1 :foo (->A)) (->A))))
  (is (= :A ((lp/find-protocol-method P2 :foo (->A)) (->A))))
  )
