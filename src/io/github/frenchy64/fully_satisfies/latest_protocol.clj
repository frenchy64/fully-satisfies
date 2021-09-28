;; TODO unit test
(ns io.github.frenchy64.fully-satisfies.latest-protocol
  (:refer-clojure :exclude [satisfies? find-protocol-impl find-protocol-method extends?])
  (:require [clojure.core :as cc]))

(def ^:private -latest (comp deref :var))

(defn satisfies-latest?
  "Returns true if x satisfies the latest version of the protocol"
  ([p v]
   (satisfies-latest? cc/satisfies?
                      p
                      v))
  ([satisfies? p v]
   (satisfies? (-latest p) v)))

(def satisfies? satisfies-latest?)

(defn find-latest-protocol-impl
  "Returns true if atype extends the latest version of the protocol"
  [protocol v]
  (cc/find-protocol-impl (-latest p) v))

(def find-protocol-impl find-latest-protocol-impl)

(defn extends-latest? [protocol atype]
  (cc/extends? (-latest p) atype))

(def extends? extends-latest?)

(defn latest-extenders 
  "Returns a collection of the types explicitly extending the latest version of the protocol"
  [protocol atype]
  (cc/extenders protocol atype))

(def extenders latest-extenders)
