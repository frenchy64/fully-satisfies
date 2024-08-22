(ns io.github.frenchy64.fully-satisfies.configurable-core-macros-test
  (:refer-clojure :exclude [replace])
  (:require [clojure.test :refer [is]]
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

;;TODO generate requires
(defn ->clojure-core*
  "
  :exclude #{`defn `fn} ;;todo
  :rename {`fn `myfn}
  :replace {`fn `already-existing-fn}
  "
  [opts]
  `(do ~@(keep (fn [[sym d]]
                 (when (u/define? sym opts)
                   (macroexpand-1 (list d opts))))
               ;; TODO sort by dependency order
               core-sym->definer)))

(defmacro ->clojure-core
  "
  :exclude #{`defn `fn} ;;todo
  :rename {`fn `myfn}
  :replace {`fn `already-existing-fn}
  "
  [opts]
  (->clojure-core* opts))

;;; tests

(def opts {:rename {`let 'my-let
                    `fn 'my-fn
                    `defn 'my-defn}})

(->clojure-core `opts)
(comment
  (spit "test/io/github/frenchy64/fully_satisfies/configurable_core_macros_test_generated.clj"
        (with-out-str
          (println "(ns io.github.frenchy64.fully-satisfies.configurable-core-macros-test-generated)")
          (binding [*print-meta* true]
            (clojure.pprint/pprint (->clojure-core* `opts)))))
  (eval
    (-> (with-out-str
          (binding [*print-meta* true]
            (clojure.pprint/pprint (->clojure-core* `opts))))
        read-string)))

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
