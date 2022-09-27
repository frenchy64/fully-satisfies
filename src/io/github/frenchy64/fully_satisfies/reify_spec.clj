(ns io.github.frenchy64.fully-satisfies.reify-spec
  (:require [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as cs]))

(s/def ::reify-args
  (s/cat :options (s/keys*)
         :specs (s/* (s/cat :name symbol?
                            :methods (s/* (s/spec
                                            (s/cat :name simple-symbol?
                                                   :params (s/and vector?
                                                                  (s/cat :params (s/+ ::cs/binding-form)))
                                                   :body (s/alt :prepost+body (s/cat :prepost map?
                                                                                     :body (s/+ any?))
                                                                :body (s/* any?)))))))))
