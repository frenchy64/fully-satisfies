(ns com.ambrose.fully-satisfies-test
  (:require [clojure.test :refer :all]
            [com.ambrose.fully-satisfies :refer :all]))

(defprotocol A
  (aA [this])
  (bA [this]))

(deftype NoExtended [])
(deftype Extended [])
(deftype ExtendedA [])
(deftype ExtendedAB [])
(extend-protocol A
  Extended
  (aA [_])
  ExtendedA
  (aA [_])
  ExtendedAB
  (aA [_])
  (bA [_]))

(defprotocol PWithPartialNilImpl
  (aPWithPartialNilImpl [this])
  (bPWithPartialNilImpl [this]))

(extend-protocol PWithPartialNilImpl
  nil
  (aPWithPartialNilImpl [this]))

(defprotocol PWithFullNilImpl
  (aPWithFullNilImpl [this])
  (bPWithFullNilImpl [this]))

(extend-protocol PWithFullNilImpl
  nil
  (aPWithFullNilImpl [this])
  (bPWithFullNilImpl [this]))

(deftest fully-satisfies?-test
  ;; implemented directly
  (is (not (fully-satisfies? A (reify))))
  (is (not (fully-satisfies? A (reify A))))
  (is (not (fully-satisfies? A (reify A (aA [this])))))
  (is (fully-satisfies? A (reify A (aA [this]) (bA [this]))))
  ;; via extend
  (is (not (fully-satisfies? A (->NoExtended))))
  (is (not (fully-satisfies? A (->Extended))))
  (is (not (fully-satisfies? A (->ExtendedA))))
  (is (fully-satisfies? A (->ExtendedAB)))
  ;; nil
  (is (not (fully-satisfies? A nil)))
  (is (not (fully-satisfies? PWithPartialNilImpl nil)))
  (is (fully-satisfies? PWithFullNilImpl nil))
  )
