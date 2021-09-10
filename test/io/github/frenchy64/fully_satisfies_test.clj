(ns io.github.frenchy64.fully-satisfies-test
  (:require [clojure.test :refer :all]
            [io.github.frenchy64.fully-satisfies :refer :all]))

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

(defprotocol PWithPartialObjectImpl
  (aPWithPartialObjectImpl [this])
  (bPWithPartialObjectImpl [this]))

(extend-protocol PWithPartialObjectImpl
  Object
  (aPWithPartialObjectImpl [this] :default))

(defprotocol PWithFullObjectImpl
  (aPWithFullObjectImpl [this])
  (bPWithFullObjectImpl [this]))

(extend-protocol PWithFullObjectImpl
  Object
  (aPWithFullObjectImpl [this])
  (bPWithFullObjectImpl [this]))

(defprotocol PExtendViaMetadata
  :extend-via-metadata true
  (aPExtendViaMetadata [this])
  (bPExtendViaMetadata [this]))

(defprotocol PExtendViaMetadataWithPartialObjectImpl
  :extend-via-metadata true
  (aPExtendViaMetadataWithPartialObjectImpl [this])
  (bPExtendViaMetadataWithPartialObjectImpl [this]))

(extend-protocol PExtendViaMetadataWithPartialObjectImpl
  Object
  (aPExtendViaMetadataWithPartialObjectImpl [this] :default))

(defprotocol PExtendViaMetadataWithFullObjectImpl
  :extend-via-metadata true
  (aPExtendViaMetadataWithFullObjectImpl [this])
  (bPExtendViaMetadataWithFullObjectImpl [this]))

(extend-protocol PExtendViaMetadataWithFullObjectImpl
  Object
  (aPExtendViaMetadataWithFullObjectImpl [this] :default)
  (bPExtendViaMetadataWithFullObjectImpl [this] :default))

(defrecord AssumptionExtendsZeroMethodsPExtendViaMetadata
  [])
(defrecord AssumptionExtendsAPExtendViaMetadata
  [])
(extend-protocol PExtendViaMetadata
  AssumptionExtendsZeroMethodsPExtendViaMetadata
  AssumptionExtendsAPExtendViaMetadata
  (aPExtendViaMetadata [this] :extend))

(defprotocol PNumberPartialExtended
  (aPNumberPartialExtended [this])
  (bPNumberPartialExtended [this]))

(extend-protocol PNumberPartialExtended
  Number
  (aPNumberPartialExtended [this] :extend-number))

(defprotocol PNumberFullyExtended
  (aPNumberFullyExtended [this])
  (bPNumberFullyExtended [this]))

(extend-protocol PNumberFullyExtended
  Number
  (aPNumberFullyExtended [this] :extend-number)
  (bPNumberFullyExtended [this] :extend-number))

;; TODO test abstract class implementing interface used as super. ensures
;; we use the correct getMethods vs getDeclaredMethods.
;; TODO test omitted implements shadows meta
(deftest fully-satisfies?-test
  ;; implemented directly
  (is (not (fully-satisfies? A (reify))))
  (is (not (fully-satisfies? A (reify A))))
  (is (not (fully-satisfies? A (reify A (aA [this])))))
  (is (fully-satisfies? A (reify A (aA [this]) (bA [this]))))
  ;; partially implemented directly with a complete Object impl
  (is (not (fully-satisfies? PWithFullObjectImpl (reify PWithFullObjectImpl (aPWithFullObjectImpl [this])))))
  ;; via extend
  (is (not (fully-satisfies? A (->NoExtended))))
  (is (not (fully-satisfies? A (->Extended))))
  (is (not (fully-satisfies? A (->ExtendedA))))
  (is (fully-satisfies? A (->ExtendedAB)))
  ;; nil
  (is (not (fully-satisfies? A nil)))
  (is (not (fully-satisfies? PWithPartialNilImpl nil)))
  (is (fully-satisfies? PWithFullNilImpl nil))
  (is (not (fully-satisfies? PWithPartialObjectImpl nil)))
  (is (not (fully-satisfies? PWithFullObjectImpl nil)))
  ;; Object
  (is (not (fully-satisfies? A (reify))))
  (is (not (fully-satisfies? PWithPartialObjectImpl (reify))))
  (is (fully-satisfies? PWithFullObjectImpl (reify)))
  (is (fully-satisfies? PWithFullObjectImpl (Object.)))
  (is (not (fully-satisfies? PWithPartialNilImpl (reify))))
  (is (not (fully-satisfies? PWithFullNilImpl (reify))))
  (is (not (fully-satisfies? PWithPartialNilImpl (Object.))))
  (is (not (fully-satisfies? PWithFullNilImpl (Object.))))
  ;; :extend-via-metadata
  (is (not (fully-satisfies? A (with-meta {}
                                          {`aA (fn [this])
                                           `bA (fn [this])}))))
  (is (not (fully-satisfies? PExtendViaMetadata
                             (with-meta {}
                                        {`aPExtendViaMetadata (fn [this])}))))
  (is (fully-satisfies? PExtendViaMetadata
                        (with-meta {}
                                   {`aPExtendViaMetadata (fn [this])
                                    `bPExtendViaMetadata (fn [this])})))
  ;; :extend-via-metadata + Object impl
  (is (fully-satisfies? PExtendViaMetadataWithFullObjectImpl
                        (with-meta {}
                                   {`aPExtendViaMetadataWithFullObjectImpl (fn [this])})))
  ;;    no `b` impl
  (is (not (fully-satisfies? PExtendViaMetadataWithPartialObjectImpl
                             (with-meta {}
                                        {`aPExtendViaMetadataWithPartialObjectImpl (fn [this])}))))
  ;;   `a` implemented by Object
  (is (fully-satisfies? PExtendViaMetadataWithPartialObjectImpl
                        (with-meta {}
                                   {`bPExtendViaMetadataWithPartialObjectImpl (fn [this])})))
  (is (fully-satisfies? PExtendViaMetadataWithFullObjectImpl
                        (with-meta {}
                                   {`aPExtendViaMetadataWithFullObjectImpl (fn [this])
                                    `bPExtendViaMetadataWithFullObjectImpl (fn [this])})))
  (is (fully-satisfies? PExtendViaMetadataWithPartialObjectImpl
                        (with-meta {}
                                   {`aPExtendViaMetadataWithPartialObjectImpl (fn [this])
                                    `bPExtendViaMetadataWithPartialObjectImpl (fn [this])})))
  ;; `a` implemented via extend, `b` via metadata
  (let [v (with-meta (->AssumptionExtendsAPExtendViaMetadata)
                     {`bPExtendViaMetadata (fn [this] :meta)})]
    (is (= :extend (aPExtendViaMetadata v)))
    (is (= :meta (bPExtendViaMetadata v)))
    (is (fully-satisfies?
          PExtendViaMetadata
          v)))
  (doseq [v [1 1.0 1/3]]
    ;; partially extended superclass != Object
    (testing v
      (is (= :extend-number (aPNumberPartialExtended v)))
      (is (thrown? IllegalArgumentException (bPNumberPartialExtended v)))
      (is (not (fully-satisfies? PNumberPartialExtended v))))
    ;; fully extended superclass != Object
    (testing v
      (is (= :extend-number (aPNumberFullyExtended v)))
      (is (= :extend-number (bPNumberFullyExtended v)))
      (is (fully-satisfies? PNumberFullyExtended v))))
  )

(deftest protocol-assumptions
  (is (= :a
         (aPExtendViaMetadataWithPartialObjectImpl
           (with-meta {}
                      {`aPExtendViaMetadataWithPartialObjectImpl (fn [this] :a)
                       `bPExtendViaMetadataWithPartialObjectImpl (fn [this] :b)}))))
  (is (= :default
         (aPExtendViaMetadataWithPartialObjectImpl
           (with-meta {}
                      {`bPExtendViaMetadataWithPartialObjectImpl (fn [this] :b)}))))
  (is (thrown? IllegalArgumentException
               (bPExtendViaMetadataWithPartialObjectImpl
                 {})))
  (testing "missing direct implementation overrides metadata"
    (is (= :a
           (aPExtendViaMetadataWithPartialObjectImpl
             (with-meta (reify
                          PExtendViaMetadataWithPartialObjectImpl
                          (aPExtendViaMetadataWithPartialObjectImpl [this] :a))
                        {`aPExtendViaMetadataWithPartialObjectImpl (fn [this] :b)}))))
    (is (thrown?
          AbstractMethodError
          (aPExtendViaMetadataWithPartialObjectImpl
            (with-meta (reify
                         PExtendViaMetadataWithPartialObjectImpl)
                       {`aPExtendViaMetadataWithPartialObjectImpl (fn [this] :b)})))))
  (testing "missing extends implementation drops to metadata"
    ;; sanity check
    (is (thrown?
          IllegalArgumentException
          (aPExtendViaMetadata
            (->AssumptionExtendsZeroMethodsPExtendViaMetadata))))
    (is (= :b
           (aPExtendViaMetadata
             (with-meta (->AssumptionExtendsZeroMethodsPExtendViaMetadata)
                        {`aPExtendViaMetadata (fn [this] :b)}))))))
