(ns io.github.frenchy64.fully-satisfies.configurable-core-macros-test
  (:refer-clojure :exclude [replace])
  (:require [clojure.test :refer [is]]
            [cljfmt.core :as fmt]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.utils :as u]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.let :as let]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.fn :as fn]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.defn :as defn]))

;(alter-var-root #'*print-namespace-maps* (fn [_] false))
;(set! *print-namespace-maps* false)

;;;;;;;;;;;;;;;;;;;;
;; High-level API
;;;;;;;;;;;;;;;;;;;;

(def core-sym->definer
  [[`let `let/->let]
   [`fn `fn/->fn]
   [`defn `defn/->defn]])

(defn flatten-top-level-forms [form]
  (let [rec (fn rec [form]
              (if (and (seq? form)
                       (= 'do (first form))
                       (next form))
                (eduction (mapcat rec) (rest form))
                [form]))]
    (vec (rec form))))

;;TODO generate requires
(defn ->clojure-core*
  "
  :exclude #{`defn `fn} ;;todo
  :rename {`fn `myfn}
  :replace {`fn `already-existing-fn}
  "
  [opts]
  (assert (not (map? opts)) (pr-str opts))
  (let [opts (u/resolve-opts opts)]
    (reduce (fn [m {:keys [forms requires]}]
              (-> m
                  (update :forms into forms)
                  (update :requires into requires)))
            {:forms [] :requires ['[clojure.core :as cc]]}
            (eduction
              (keep (fn [[sym d]]
                      (when (u/define? sym opts)
                        {:forms (flatten-top-level-forms (macroexpand-1 (list d opts)))
                         :requires [[(-> d namespace symbol) :as (symbol (name sym))]]})))
              ;; TODO sort by dependency order
              core-sym->definer))))

(defmacro ->clojure-core
  "
  :exclude #{`defn `fn} ;;todo
  :rename {`fn `myfn}
  :replace {`fn `already-existing-fn}
  "
  [opts]
  `(do ~@(:forms (->clojure-core* opts))))

(def deterministic-print-bindings
  {#'*print-level* nil
   #'*print-length* nil
   #'*print-meta* true
   #'*print-namespace-maps* false})

(defn format-forms-via-pprint [forms]
  (apply str (interpose
               "\n"
               (map (fn [form]
                      (with-bindings deterministic-print-bindings
                        (with-out-str
                          (clojure.pprint/pprint form))))
                    forms))))

(defn format-forms-via-cljfmt [forms]
  (-> forms
      format-forms-via-pprint
      fmt/reformat-string))

(defn format-forms-via-zprint [forms]
  (zp/zprint-file-str
    (apply str (interpose "\n\n"
                          (map #(with-bindings deterministic-print-bindings
                                  (pr-str %))
                               forms)))
    ""
    ""
    {}))

(defn str-clojure-core-variant [nsym macro-opts {:keys [formatting-lib]}]
  (let [{:keys [forms requires]} (->clojure-core* macro-opts)
        top-level-forms (cons (list* 'ns nsym
                                     (when (seq requires)
                                       [(list* :require requires)]))
                              forms)]
    ((case formatting-lib
       :pprint format-forms-via-pprint
       :cljfmt format-forms-via-cljfmt
       :zprint format-forms-via-zprint)
      top-level-forms)))

(defn print-clojure-core-variant [nsym macro-opts print-opts]
  (print (str-clojure-core-variant nsym macro-opts print-opts)))

(defn spit-clojure-core-variant [file nsym macro-opts {:keys [formatting-lib] :as print-opts}]
  {:pre [(string? file)
         (simple-symbol? nsym)]}
  (spit file (str ";; formatted by " formatting-lib "\n"
                  (str-clojure-core-variant nsym macro-opts print-opts))))

;;; tests

(def opts {:rename {`let 'my-let
                    `fn 'my-fn
                    `defn 'my-defn}})

(->clojure-core `opts)

(comment
  (print-clojure-core-variant
    'io.github.frenchy64.fully-satisfies.configurable-core-macros-test-generated
    `opts
    {:formatting-lib :pprint})
  (doseq [lib [:pprint :cljfmt :zprint]]
    (spit-clojure-core-variant
      (format "test/io/github/frenchy64/fully_satisfies/configurable_core_macros_test_generated_%s.clj"
              (name lib))
      (symbol (str "io.github.frenchy64.fully-satisfies.configurable-core-macros-test-generated-" (name lib)))
      `opts
      {:formatting-lib lib})))

(my-defn f [&form])
(my-defn g ([&form &env]) ([&form &env arg]))
(my-defn my-identity [x] x)

(deftest ->fn-test
  (is (= 1 (my-identity 1))))

(deftest ->defn-test
  (testing "fixes Clojure bug" ;; https://clojure.atlassian.net/browse/CLJ-2874
    (is (= '([&form]) (-> #'f meta :arglists)))
    (is (= '([&form &env] [&form &env arg]) (-> #'g meta :arglists))))
  (is (= 1 (my-identity 1))))
