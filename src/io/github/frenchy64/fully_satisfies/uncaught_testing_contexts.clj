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

  With clojure.test/{deftest,testing} 1.10.3:

    ;=> (test-var #'my-test)
    ERROR in (my-test)
    Uncaught exception, not in assertion.
    expected: nil
    actual: java.lang.AssertionError: Assert failed: false
    ...
      
  With alternatives in this namespace (notice `foo 2` is mentioned):

    ;=> (test-var #'my-test)
    ERROR in (my-test)
    Uncaught exception, possibly thrown in testing context: foo 2
    expected: nil
    actual: java.lang.AssertionError: Assert failed: false
    ..."
  (:require [clojure.test :as t]))

(def ^:dynamic *exceptional-testing-contexts* nil) ; bound to the last t/*testing-contexts* that did not finished executing

(defn record-uncaught-exception-contexts
  "Call when an exception is thrown in a testing context 
  to record the most likely *testing-contexts* to report
  in *exceptional-testing-contexts*."
  []
  (when (thread-bound? #'*exceptional-testing-contexts*)
    (let [etc (vec *exceptional-testing-contexts*)
          tc (vec t/*testing-contexts*)]
      (when (or ;; prefer longer contexts
                (<= (count etc) (count tc))
                ;; heuristically ignore context changes due to stack unwinding.
                ;; downside: cannot distinguish between normal unwinding
                ;;  (testing "a" (testing "b" (throw ...)))
                ;; and throw during unwinding
                ;;  (testing "a" (try (testing "b" (throw ...))
                ;;                 (catch ... (throw ...))))
                ;; "a b" is suggested as the exceptional context in both cases.
                (not (= tc (subvec etc
                                   (- (count etc) (count tc))
                                   (count etc)))))
        (try (set! *exceptional-testing-contexts* t/*testing-contexts*)
             ;; thread-bound? returns true with implicit binding conveyance even though you
             ;; can't set! from non-binding thread.
             ;; see https://clojure.atlassian.net/browse/CLJ-1077
             (catch IllegalStateException _))))))

(defn report-uncaught-exception
  "Report an uncaught exception using *exceptional-testing-contexts* to
  guess the most helpful message."
  [e]
  (t/do-report {:type :error, :message (if-some [etc *exceptional-testing-contexts*]
                                         (str "Uncaught exception, possibly thrown in testing context: "
                                              (binding [t/*testing-contexts* etc]
                                                (t/testing-contexts-str)))
                                         "Uncaught exception, not in assertion.")
                :expected nil, :actual e}))

(defmacro testing
  "Like clojure.test/testing, except records testing contexts on
  uncaught exceptions.
 
  Use in conjunction with `deftest` in this namespace."
  [string & body]
  `(binding [t/*testing-contexts* (conj t/*testing-contexts* ~string)]
     (try (do ~@body) ;; don't let body leak catch/finally syntax
          (catch Throwable e#
            ;; `resolve` for forward compatibility, eg., avoid https://clojure.atlassian.net/browse/CLJ-2564?focusedCommentId=48791
            (when-some [f# (resolve '~`record-uncaught-exception-contexts)]
              (f#))
            (throw e#)))))

(defmacro deftest
  "Like clojure.test/deftest, except swallows uncaught exceptions
  and reports them as test errors (with improved error messages via
  *exceptional-testing-contexts*). This is normally done by `test-var`,
  with an unhelpful error message.

  Use in conjuction with `testing` in this namespace."
  [name & body]
  (when *load-tests*
    `(def ~(vary-meta name assoc :test `(fn []
                                          (try (do ~@body)
                                               (catch Throwable e#
                                                 (report-uncaught-exception e#)))))
          (fn [] (t/test-var (var ~name))))))
