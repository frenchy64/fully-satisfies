;; TODO unit test
(ns io.github.frenchy64.fully-satisfies.latest-protocol
  (:refer-clojure :exclude [satisfies? find-protocol-impl find-protocol-method extends? extenders])
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
  [protocol v]
  (cc/find-protocol-impl (-latest protocol) v))

(def find-protocol-impl find-latest-protocol-impl)

(defn find-latest-protocol-method [protocol methodk x]
  (get (find-latest-protocol-impl protocol x) methodk))

(def find-protocol-method find-latest-protocol-method)

(defn extends-latest?
  "Returns true if atype extends the latest version of the protocol"
  [protocol atype]
  (cc/extends? (-latest protocol) atype))

(def extends? extends-latest?)

(defn latest-extenders 
  "Returns a collection of the types explicitly extending the latest version of the protocol"
  [protocol]
  (cc/extenders (-latest protocol)))

(def extenders latest-extenders)
