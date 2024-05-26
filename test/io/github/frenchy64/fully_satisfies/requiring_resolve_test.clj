;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.requiring-resolve-test
  (:refer-clojure :exclude [requiring-resolve])
  (:require [clojure.test :refer [is]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts :refer [deftest testing]]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]))

(def ^:dynamic *coord* nil)

(deftest requiring-resolve-atomic-load-test
  (remove-ns 'io.github.frenchy64.fully-satisfies.requiring-resolve-test.ns-libs-requiring-resolve-atomic)
  (dosync (alter @#'clojure.core/*loaded-libs* disj 'io.github.frenchy64.fully-satisfies.requiring-resolve-test.ns-libs-requiring-resolve-atomic))
  (let [start-right (promise)
        right-started (promise)
        finish-loading (promise)
        right-load (atom false)
        _ (is (not (find-ns 'io.github.frenchy64.fully-satisfies.requiring-resolve-test.ns-libs-requiring-resolve-atomic)))
        _ (is (not (contains? @@#'clojure.core/*loaded-libs* 'io.github.frenchy64.fully-satisfies.requiring-resolve-test.ns-libs-requiring-resolve-atomic)))
        left (future
               (with-bindings {;#'*loading-verbosely* true
                               #'*coord* (fn []
                                           (deliver start-right true)
                                           @finish-loading)}
                 @(requiring-resolve 'io.github.frenchy64.fully-satisfies.requiring-resolve-test.ns-libs-requiring-resolve-atomic/a)))
        right (future
                @start-right
                (with-bindings {;#'*loading-verbosely* true
                                #'*coord* (fn [] (reset! right-load true))}
                  (deliver right-started true)
                  @(requiring-resolve 'io.github.frenchy64.fully-satisfies.requiring-resolve-test.ns-libs-requiring-resolve-atomic/a)))]
    (try (is (= true (deref start-right 5000 ::blocked)))
         (is (= true (deref right-started 5000 ::blocked)))
         (is (= ::blocked (deref left 2000 ::blocked)))
         (is (= ::blocked (deref right 2000 ::blocked)))
         (is (= ::blocked (deref finish-loading 2000 ::blocked)))
         (is (not @right-load))
         (is (find-ns 'io.github.frenchy64.fully-satisfies.requiring-resolve-test.ns-libs-requiring-resolve-atomic))
         (is (not (contains? @@#'clojure.core/*loaded-libs* 'io.github.frenchy64.fully-satisfies.requiring-resolve-test.ns-libs-requiring-resolve-atomic)))
         (deliver finish-loading true)
         (is (= 2 (deref left 1000 ::left-blocked)))
         (is (= 2 (deref right 1000 ::right-blocked)))
         (is (not @right-load))
         (is (find-ns 'io.github.frenchy64.fully-satisfies.requiring-resolve-test.ns-libs-requiring-resolve-atomic))
         (is (contains? @@#'clojure.core/*loaded-libs* 'io.github.frenchy64.fully-satisfies.requiring-resolve-test.ns-libs-requiring-resolve-atomic))
         (finally
           (run! #(deliver % true) [start-right right-started finish-loading])))))

(deftest self-requiring-resolve-test
  (remove-ns 'io.github.frenchy64.fully-satisfies.requiring-resolve-test.ns-libs-requiring-resolve-self)
  (dosync (alter @#'clojure.core/*loaded-libs* disj 'io.github.frenchy64.fully-satisfies.requiring-resolve-test.ns-libs-requiring-resolve-self))
  (is (nil? (resolve 'io.github.frenchy64.fully-satisfies.requiring-resolve-test.ns-libs-requiring-resolve-self/a)))
  (require 'io.github.frenchy64.fully-satisfies.requiring-resolve-test.ns-libs-requiring-resolve-self)
  (is (= 1 @(resolve 'io.github.frenchy64.fully-satisfies.requiring-resolve-test.ns-libs-requiring-resolve-self/a))))

(deftest repl-requiring-resolve-test
  (let [temp-ns (gensym)]
    (binding [*ns* *ns*]
      (eval (list `ns temp-ns))
      (is (nil? (requiring-resolve (symbol (str temp-ns) "missing")))))))
