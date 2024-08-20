(ns io.github.frenchy64.fully-satisfies.configurable-core-macros
  (:require [io.github.frenchy64.fully-satisfies.configurable-core-macros.defmacro :as defmacro]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.defmethod :as defmethod]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.defn :as defn]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.fn :as fn]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.if-let :as if-let]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.let :as let]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.utils :as u]))

(def ^:private core-sym-infos
  [let/info
   fn/info
   defn/info
   defmacro/info
   defmethod/info
   if-let/info])

(defn- flatten-top-level-forms [form]
  (let [rec (fn rec [form]
              (if (and (seq? form)
                       (= 'do (first form))
                       (next form))
                (eduction (mapcat rec) (rest form))
                [form]))]
    (vec (rec form))))

(defn- ->clojure-core*
  "
  :exclude #{`defn `fn} ;;todo
  :rename {`fn `myfn}
  :replace {`fn `already-existing-fn}
  "
  [opts]
  (assert (not (map? opts)) (pr-str opts))
  (let [opts (u/resolve-opts opts)]
    (-> (reduce (fn [m {:keys [forms requires]}]
              (-> m
                  (update :forms into forms)
                  (update :requires into (distinct requires))))
            {:forms [] :requires ['[clojure.core :as cc]]}
            (eduction
              (keep (fn [{:keys [sym ctor requires]}]
                      (when (u/define? sym opts)
                        {:forms (flatten-top-level-forms (macroexpand-1 (list ctor opts)))
                         :requires (into [(-> ctor namespace symbol)]
                                         requires)})))
              ;; TODO sort by dependency order
              core-sym-infos))
        (update :requires #(into [] (distinct) %)))))

(defmacro ->clojure-core
  "
  :exclude #{`defn `fn} ;;todo
  :rename {`fn `myfn}
  :replace {`fn `already-existing-fn}
  "
  [opts]
  `(do ~@(:forms (->clojure-core* opts))))
