(ns io.github.frenchy64.fully-satisfies.partially-satisfies-test
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [testing deftest]]
            [io.github.frenchy64.fully-satisfies.partially-satisfies :refer [partially-satisfies?] :as ps]))

(defn te* [top-levels body]
  (let [g (gensym "io.github.frenchy64.fully-satisfies.partially-satisfies-test.gensym")]
    ((binding [*ns* *ns*]
       (eval `(do (ns ~g
                    (:require ~'[clojure.test :refer :all]
                              ~'[io.github.frenchy64.fully-satisfies.partially-satisfies :as ps :refer [partially-satisfies?]]))
                  ~@top-levels
                  (fn [] (do ~@body))))))
    (remove-ns g)
    nil))

(defmacro te [top-levels & body]
  `(te* '~top-levels '~body))

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

(definterface IInterface)

(defprotocol PIInterfaceExtendViaMetaPartialExtended
  :extend-via-metadata true
  (aPIInterfaceExtendViaMetaPartialExtended [this])
  (bPIInterfaceExtendViaMetaPartialExtended [this])
  (cPIInterfaceExtendViaMetaPartialExtended [this]))

(extend-protocol PIInterfaceExtendViaMetaPartialExtended
  IInterface
  (aPIInterfaceExtendViaMetaPartialExtended [this] :extend))

(defprotocol PIInterfaceExtendViaMetaFullyExtended
  :extend-via-metadata true
  (aPIInterfaceExtendViaMetaFullyExtended [this])
  (bPIInterfaceExtendViaMetaFullyExtended [this]))

(extend-protocol PIInterfaceExtendViaMetaFullyExtended
  IInterface
  (aPIInterfaceExtendViaMetaFullyExtended [this] :extend)
  (bPIInterfaceExtendViaMetaFullyExtended [this] :extend))

(deftest partially-satisfies?-test
  (te [(defprotocol A)
       (deftype T1 [] A)
       (deftype T2 [])]
      (doseq [partially-satisfies? [partially-satisfies? ps/satisfies?]]
        (testing partially-satisfies?
          (is (partially-satisfies? A (->T1)))
          (is (not (partially-satisfies? A (->T2)))))))
  (te [(defprotocol A
         :extend-via-metadata true)
       (defrecord T1 [] A)
       (defrecord T2 []) ]
      (doseq [partially-satisfies? [partially-satisfies? ps/satisfies?]]
        (is (partially-satisfies? A (->T1)))
        (is (not (partially-satisfies? A (->T2))))))
  (te [(defprotocol A
         :extend-via-metadata true
         (foo [this]))
       (defrecord T1 [] A)
       (defrecord T2 [])
       (def this-nstr (-> *ns* ns-name name))]
      (doseq [partially-satisfies? [partially-satisfies? ps/satisfies?]]
        (is (partially-satisfies? A (->T1)))
        (is (not (partially-satisfies? A (->T2))))
        (is (partially-satisfies? A (with-meta (->T2)
                                               {(symbol this-nstr "foo") identity}))))))
