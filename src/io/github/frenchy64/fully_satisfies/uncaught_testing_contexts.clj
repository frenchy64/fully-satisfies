;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.uncaught-testing-contexts
  "Drop-in replacements for `clojure.test/{deftest,testing}` that (when used
  together) enhances uncaught exception error messages with the (most likely)
  testing context it was thrown from.
  
  Example:

    (deftest my-test
      (testing \"foo\"
        (doseq [v [1 2 3]]
          (testing v
            (assert (= 1 v))))))

  With clojure.test/{deftest,testing} 1.10.3 (notice `foo 2` is not mentioned):

    user=> (test-var #'my-test)
    ;ERROR in (my-test)
    ;Uncaught exception, not in assertion.
    ;expected: nil
    ;actual: java.lang.AssertionError: Assert failed: false
    ;...
      
  With {deftest,testing} in this namespace (notice `foo 2` is mentioned):

    user=> (test-var #'my-test)
    ;ERROR in (my-test)
    ;Uncaught exception, possibly thrown in testing context: foo 2
    ;expected: nil
    ;actual: java.lang.AssertionError: Assert failed: false
    ;..."
  (:require [clojure.stacktrace :as stack]
            [clojure.string :as str]
            [clojure.test :as t]))

; bound to an atom that contains the last t/*testing-contexts* that did not finished executing.
; an atom so tests can be delegated to other threads via binding conveyance.
(def ^:dynamic *exceptional-testing-contexts* nil)

(defn record-uncaught-exception-contexts
  "Call when an exception is thrown in a testing context 
  to record the most likely *testing-contexts* to report
  in *exceptional-testing-contexts*."
  [e]
  (some-> *exceptional-testing-contexts*
          (swap! update (stack/root-cause e) #(or % t/*testing-contexts*))))

(defn report-uncaught-exception
  "Report an uncaught exception using *exceptional-testing-contexts* to
  guess the most helpful message."
  [e]
  (let [root (stack/root-cause e)]
    (t/do-report {:type :error, :message (if-some [{etc root :as e->etc} (some-> *exceptional-testing-contexts* deref not-empty)]
                                           (str "Uncaught exception, thrown in testing context: "
                                                (binding [t/*testing-contexts* etc]
                                                  (t/testing-contexts-str))
                                                (when-some [rest-etcs (some-> e->etc (dissoc root) not-empty vals (->> (mapv vec)) sort (->> (mapv list*)))]
                                                  ;; might want to print exception msg to help disambiguate?
                                                  (str "\n\nAlso found uncaught exceptions in the following testing contexts: "
                                                       (apply str
                                                              (str/join "\n"
                                                                        (map (fn [etc]
                                                                               (binding [t/*testing-contexts* etc]
                                                                                 (t/testing-contexts-str)))
                                                                             rest-etcs))))))
                                           "Uncaught exception, not in assertion.")
                  :expected nil, :actual e})))

(defmacro testing+record-uncaught-contexts
  "Like clojure.test/testing, except records testing contexts on
  uncaught exceptions.
 
  Use in conjunction with `deftest` in this namespace."
  [string & body]
  `(binding [t/*testing-contexts* (conj t/*testing-contexts* ~string)]
     (try (do ~@body) ;; don't let body leak catch/finally syntax (excluded from upstream patch)
          (catch Throwable e#
            ;; `resolve` for forward compatibility, eg., avoid https://clojure.atlassian.net/browse/CLJ-2564?focusedCommentId=48791
            (when-some [f# (resolve '~`record-uncaught-exception-contexts)]
              (f# e#))
            (throw e#)))))

(defmacro testing [& args] `(testing+record-uncaught-contexts ~@args))

;; a function so `resolve` can be used 
(defn -run-test-body
  "For libraries that mimic clojure.test's API. f
  should be a thunk that runs the test."
  [f]
  (binding [*exceptional-testing-contexts* (atom {})]
    (try (f)
         (catch Throwable e
           (report-uncaught-exception e)))))

(defmacro deftest+report-uncaught-contexts
  "Like clojure.test/deftest, except swallows uncaught exceptions
  and reports them as test errors (with improved error messages via
  *exceptional-testing-contexts*). This is normally done by `test-var`,
  with an unhelpful error message.

  Use in conjunction with `testing` in this namespace."
  [name & body]
  (when t/*load-tests*
    `(def ~(vary-meta name assoc :test `(fn []
                                          ;;move to `test-var` when proposing upstream
                                          (-run-test-body #(do ~@body))))
          (fn [] (t/test-var (var ~name))))))

(defmacro deftest [& args] `(deftest+report-uncaught-contexts ~@args))
