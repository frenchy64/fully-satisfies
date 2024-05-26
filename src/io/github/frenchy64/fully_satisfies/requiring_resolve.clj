;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.requiring-resolve
  "A thread-safe version of requiring-resolve.
  
  See https://clojure.atlassian.net/browse/CLJ-2735
  
  This may be fixed in future versions of clojure.
  For versions 1.10 to 1.12.0-alpha9, the `requiring-resolve` var in this namespace will use a compatible thread-safe implementation.

  For other versions, a compile-time error will occur. You must resolve it by either setting:
  1) -Dio.github.frenchy64.fully-satisfies.requiring-resolve=patch if CLJ-2735 is not fixed in this version of Clojure and the thread-safe variant in this namespace is compatible with this version of Clojure, or
  2) -Dio.github.frenchy64.fully-satisfies.requiring-resolve=clojure.core/requiring-resolve if CLJ-2735 is fixed in this version of Clojure
  3) -Dio.github.frenchy64.fully-satisfies.requiring-resolve=<fully-qualified-var> if CLJ-2735 is not fixed in this version of Clojure and fully-qualified-var should be used to patch it
  
  Please report such errors to https://github.com/frenchy64/fully-satisfies"
  (:refer-clojure :exclude [requiring-resolve thread-safe-requiring-resolve known-broken-clojure-versions load-one
                            load-libs load-lib load-all])
  (:require [clojure.core :as cc]))

(def ^:private known-broken-clojure-versions
  (-> #{}
      (into (map #(do {:major 1, :minor 10, :incremental %, :qualifier nil})) (range 4))
      (into (map #(do {:major 1, :minor 11, :incremental %, :qualifier nil})) (range 4))
      (into (map #(do {:major 1, :minor 12, :incremental 0, :qualifier (str "alpha" %)})) (range 13))))

(defn- load-one
  "Loads a lib given its name. If need-ns, ensures that the associated
  namespace exists after loading. Records the load after
  loading successfully so any duplicate loads can be skipped."
  [lib need-ns]
  (let [loaded (with-bindings {#'cc/*loaded-libs* (ref (conj @@#'cc/*loaded-libs* lib))}
                 (load (#'cc/root-resource lib))
                 @@#'cc/*loaded-libs*)]
    (#'cc/throw-if (and need-ns (not (find-ns lib)))
                   "namespace '%s' not found after loading '%s'"
                   lib (#'cc/root-resource lib))
    (let [llibs @#'cc/*loaded-libs*
          llibs-global (.getRawRoot #'cc/*loaded-libs*)]
      (dosync
       (commute llibs into loaded)
       (when-not (identical? llibs llibs-global)
         (commute llibs-global into loaded))))))

(defn- load-all
  "Loads a lib given its name and forces a load of any libs it directly or
  indirectly loads. If need-ns, ensures that the associated namespace
  exists after loading. Records the load after loading successfully so any
  duplicate loads can be skipped."
  [lib need-ns]
  (let [loaded (with-bindings {#'cc/*loaded-libs* (ref (sorted-set))}
                 (load-one lib need-ns)
                 @@#'cc/*loaded-libs*)]
    (dosync
     (commute @#'cc/*loaded-libs* into loaded))))

(defn- load-lib
  "Loads a lib with options"
  [prefix lib & options]
  (#'cc/throw-if (and prefix (pos? (.indexOf (name lib) (int \.))))
                 "Found lib name '%s' containing period with prefix '%s'.  lib names inside prefix lists must not contain periods"
                 (name lib) prefix)
  (let [lib (if prefix (symbol (str prefix \. lib)) lib)
        opts (apply hash-map options)
        {:keys [as reload reload-all use verbose as-alias]} opts
        loaded (contains? @@#'cc/*loaded-libs* lib)
        need-ns (or as use)
        load (cond reload-all load-all
                   reload load-one
                   (not loaded) (cond need-ns load-one
                                      as-alias (fn [lib _need] (create-ns lib))
                                      :else load-one))

        filter-opts (select-keys opts '(:exclude :only :rename :refer))
        undefined-on-entry (not (find-ns lib))]
    (with-bindings {#'cc/*loading-verbosely* (or @#'cc/*loading-verbosely* verbose)}
      (if load
        (try
          (load lib need-ns)
          (catch Exception e
            (when undefined-on-entry
              (remove-ns lib))
            (throw e)))
        (#'cc/throw-if (and need-ns (not (find-ns lib)))
                       "namespace '%s' not found" lib))
      (when (and need-ns #'cc/*loading-verbosely*)
        (printf "(clojure.core/in-ns '%s)\n" (ns-name *ns*)))
      (when as
        (when #'cc/*loading-verbosely*
          (printf "(clojure.core/alias '%s '%s)\n" as lib))
        (alias as lib))
      (when as-alias
        (when #'cc/*loading-verbosely*
          (printf "(clojure.core/alias '%s '%s)\n" as-alias lib))
        (alias as-alias lib))
      (when (or use (:refer filter-opts))
        (when #'cc/*loading-verbosely*
          (printf "(clojure.core/refer '%s" lib)
          (doseq [opt filter-opts]
            (printf " %s '%s" (key opt) (print-str (val opt))))
          (printf ")\n"))
        (apply refer lib (mapcat seq filter-opts))))))

(defn- thread-safe-requiring-resolve
  "Resolves namespace-qualified sym after ensuring sym's namespace is loaded.
  Thread-safe with other calls to this function and clojure.core/requiring-resolve.
  Not thread-safe with simultaneous calls to clojure.core/require."
  [sym]
  (if (qualified-symbol? sym)
    (let [nsym (-> sym namespace symbol)]
      (when-not (contains? @(.getRawRoot #'cc/*loaded-libs*) nsym)
        (locking clojure.lang.RT/REQUIRE_LOCK
          (when-not (contains? @(.getRawRoot #'cc/*loaded-libs*) nsym)
            (load-lib nil nsym))))
      (resolve sym))
    (throw (IllegalArgumentException. (str "Not a qualified symbol: " sym)))))

(def ^:private impl-var
  (let [prop (System/getProperty "io.github.frenchy64.fully-satisfies.requiring-resolve")]
    (cc/requiring-resolve
      (case (or prop (when (known-broken-clojure-versions *clojure-version*)
                       "patch"))
        "patch" `thread-safe-requiring-resolve
        nil (throw (ex-info (str "Unknown Clojure version " (pr-str *clojure-version*) "\n"
                                 "Please set either:\n"
                                 "1) -Dio.github.frenchy64.fully-satisfies.requiring-resolve=patch if CLJ-2735 is not fixed in this version of Clojure and the thread-safe variant in io.github.frenchy64.fully-satisfies.requiring-resolve is compatible with this version of Clojure, or\n"
                                 "2) -Dio.github.frenchy64.fully-satisfies.requiring-resolve=clojure.core/requiring-resolve if CLJ-2735 is fixed in this version of Clojure\n"
                                 "3) -Dio.github.frenchy64.fully-satisfies.requiring-resolve=<fully-qualified-var> if CLJ-2735 is fixed in this version of Clojure and fully-qualified-var should be used to patch it\n"
                                 "If this is an official non-SNAPSHOT version of Clojure, please report this error to https://github.com/frenchy64/fully-satisfies")
                            {:clojure-version *clojure-version*}))
        (symbol prop)))))

(defonce
  ^{:doc (:doc (meta impl-var))
    :arglists (:arglists (meta impl-var))}
  requiring-resolve
  (if (= impl-var #'thread-safe-requiring-resolve)
    thread-safe-requiring-resolve
    (if (= impl-var #'cc/requiring-resolve)
      cc/requiring-resolve
      (fn [sym] (impl-var sym)))))
