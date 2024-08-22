(ns io.github.frenchy64.fully-satisfies.configurable-core-macros-test
  (:refer-clojure :exclude [replace])
  (:require [clojure.test :refer [is]]
            [cljfmt.core :as fmt]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.utils :as u]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.let :as let]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.fn :as fn]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.defn :as defn]))

;;;;;;;;;;;;;;;;;;;;
;; High-level API
;;;;;;;;;;;;;;;;;;;;

(def core-sym->definer
  {`let `let/->let
   `fn `fn/->fn
   `defn `defn/->defn})

(defn flatten-top-level-forms [form]
  (if (and (seq? form)
           (= 'do (first form))
           (next form))
    (eduction (mapcat flatten-top-level-forms) (rest form))
    [form]))

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
                        {:forms (vec (flatten-top-level-forms (macroexpand-1 (list d opts))))
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

(defn form-str [form]
  (binding [*print-meta* true
            *print-namespace-maps* false
            *print-level* nil
            *print-length* nil]
    (pr-str form)))

(defn str-clojure-core-variant [nsym opts]
  (let [{:keys [forms requires]} (->clojure-core* opts)
        form (into [(list* 'ns nsym
                           (when (seq requires)
                             [(list* :require requires)]))]
                   forms)]
    (fmt/reformat-string
      (apply str (interpose "\n\n" (map form-str form))))))

(defn print-clojure-core-variant [nsym opts]
  (print (str-clojure-core-variant nsym opts)))

(defn spit-clojure-core-variant [file nsym opts]
  (spit file (str-clojure-core-variant nsym opts)))

;;; tests

(def opts {:rename {`let 'my-let
                    `fn 'my-fn
                    `defn 'my-defn}})

;(->clojure-core `opts)
(comment
  (print-clojure-core-variant
    'io.github.frenchy64.fully-satisfies.configurable-core-macros-test-generated
    `opts)
  (spit-clojure-core-variant
    "test/io/github/frenchy64/fully_satisfies/configurable_core_macros_test_generated.clj"
    'io.github.frenchy64.fully-satisfies.configurable-core-macros-test-generated
    `opts))

;(my-defn f [&form])
;(my-defn g ([&form &env]) ([&form &env arg]))
;(my-defn my-identity [x] x)
;
;(deftest ->fn-test
;  (is (= 1 (my-identity 1))))
;
;(deftest ->defn-test
;  (testing "fixes Clojure bug" ;; https://clojure.atlassian.net/browse/CLJ-2874
;    (is (= '([&form]) (-> #'f meta :arglists)))
;    (is (= '([&form &env] [&form &env arg]) (-> #'g meta :arglists))))
;  (is (= 1 (my-identity 1))))
