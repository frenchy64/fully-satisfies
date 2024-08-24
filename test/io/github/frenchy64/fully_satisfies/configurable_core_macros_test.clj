(ns io.github.frenchy64.fully-satisfies.configurable-core-macros-test
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.test :refer [is]]
            [clojure.walk :as walk]
            [cljfmt.core :as fmt]
            [zprint.core :as zp]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.utils :as u]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros :as cc-macro]))

;(alter-var-root #'*print-namespace-maps* (fn [_] false))
;(set! *print-namespace-maps* false)

;;;;;;;;;;;;;;;;;;;;
;; High-level API
;;;;;;;;;;;;;;;;;;;;

(def deterministic-print-bindings
  {#'*print-level* nil
   #'*print-length* nil
   #'*print-meta* true
   #'*print-namespace-maps* false})

(defn format-forms-via-pprint [forms]
  ;; pprint cannot handle (:refer-clojure :only [])
  ;; so we print the ns form separately
  (apply str
         (with-out-str
           (with-bindings deterministic-print-bindings
             (pp/with-pprint-dispatch
               pp/simple-dispatch
               (pp/pprint (first forms)))))
         (when (next forms)
           "\n")
         (interpose
           "\n"
           (map (fn [form]
                  (with-out-str
                    (with-bindings deterministic-print-bindings
                      (pp/with-pprint-dispatch
                        pp/code-dispatch 
                        (pp/pprint form)))))
                (next forms)))))

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
  (let [{:keys [forms requires]} (#'cc-macro/->clojure-core* macro-opts)
        top-level-forms (mapv (fn postwalk [form]
                                ;; note: walk only preserves symbol metadata in 1.11!
                                (walk/postwalk
                                  (fn [form]
                                    (if (instance? clojure.lang.IObj form)
                                      (-> form
                                          (vary-meta dissoc :line :file)
                                          (vary-meta postwalk))
                                      form))
                                  form))
                              (cons (list* 'ns nsym
                                           '(:refer-clojure :only [])
                                           (when (seq requires)
                                             [(list* :require requires)]))
                                    forms))]
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
  (spit file (str (str/join "\n"
                            [";; Copyright (c) Rich Hickey. All rights reserved."
                             ";; The use and distribution terms for this software are covered by the"
                             ";; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)"
                             ";; which can be found in the file epl-v10.html at the root of this distribution."
                             ";; By using this software in any fashion, you are agreeing to be bound by"
                             ";; the terms of this license."
                             ";; You must not remove this notice, or any other, from this software."
                             ""
                             (str ";; formatted by " formatting-lib "\n")])
                  (str-clojure-core-variant nsym macro-opts print-opts))))

;;; tests

(def opts {:rename {`let 'my-let
                    `fn 'my-fn
                    `defn 'my-defn
                    `defmacro 'my-defmacro
                    `defmethod 'my-defmethod
                    `if-let 'my-if-let}})

(cc-macro/->clojure-core `opts)

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
      {:formatting-lib lib}))
  (pp/with-pprint-dispatch
    pp/code-dispatch
    (pp/pprint '(ns foo
                  (:refer-clojure :exclude [defn]))))
  #_ ;; Clojure bug
  (pp/with-pprint-dispatch
    pp/code-dispatch
    (pp/pprint '(ns foo
                  (:refer-clojure :only []))))
  )

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
