;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.configurable-core-macros.utils)

(defn resolve-opts [opts]
  {:post [(map? %)]}
  (if (map? opts)
    opts
    (let [s (if (symbol? opts)
              opts
              (let [_ (assert (= 2 (count opts)))
                    [q s] opts]
                (assert (= 'quote (first opts)))
                s))]
      (assert (qualified-symbol? s))
      @(resolve s))))

;; TODO pass opts to result somehow
(defn replacement-for [vsym opts]
  (assert (or (var? vsym)
              (qualified-symbol? vsym)))
  (let [v? (var? vsym)
        vsym (symbol vsym)
        vr (get (:replace (resolve-opts opts)) vsym vsym)]
    (assert (or (var? vr)
                (qualified-symbol? vr)))
    (cond-> vr
      v? find-var)))

(defn rename-to [vsym opts]
  {:pre [(qualified-symbol? vsym)]
   :post [(simple-symbol? %)]}
  (-> (get (:rename (resolve-opts opts)) vsym vsym)
      name
      symbol))
