(ns io.github.frenchy64.fully-satisfies.shared-protocol
  (:refer-clojure :exclude [defprotocol])
  (:require [clojure.core :as cc]))

(defmacro def-shared-protocol
  [nme & body]
  `(do (cc/defprotocol ~nme ~@body)
       (let [protocol-var# (var ~nme)]
         (-reset-methods
           (alter-var-root protocol-var#
                           update :method-builders
                           #(into {}
                                  (map (fn [[^clojure.lang.Var v# build#]]
                                         (let [cache# (clojure.lang.MethodImplCache. (symbol v#) @protocol-var# (keyword (.sym v#)))
                                               ^clojure.lang.AFunction f# (build# cache#)
                                               shared-build# (fn [cache#]
                                                               (set! (.__methodImplCache f#) cache#)
                                                               f#)]
                                           [v# shared-build#])))
                                  %))))
       '~nme))

(defmacro defprotocol [& args] `(def-shared-protocol ~@args))
