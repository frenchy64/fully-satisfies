;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.requiring-resolve
  "A variant of `clojure.core/requiring-resolve` that fixes [CLJ-2735](https://clojure.atlassian.net/browse/CLJ-2735)
  and is safe to use concurrently with other calls to itself.

  The original `requiring-resolve` implementation only works if namespaces are immutable.
  Namespaces are mutable and are incrementally mutated while its file is loaded. This mutation
  is globally visible. The problem with the original implementation of `requiring-resolve` is
  that it can return a partially loaded value before the file is fully loaded.

  For example, if two threads are both calling `requiring-resolve` on the same var, a race condition
  can occur:
  1. thread 1 calls resolve, which returns nil, acquires the require lock and starts loading the namespace
  2. thread 2 then calls resolve which succeeds in the middle of thread 1 loading the namespace.
  3. thread 2 derefs the var and calls it while thread 1 is still loading the namespace
  4. since the var is not completely loaded, the results of thread 2 are non-deterministic

  As usual, it is completely unsafe to concurrently call `require` without first acquiring REQUIRE_LOCK.

  We do not attempt to implement immutable namespaces in order to solve this problem. Instead, we
  view this a bug in `requiring-resolve`'s implementation which was advertised as atomic and thread safe
  with itself but did not account for mutable namespaces.

  This is the original implementation of requiring-resolve:

  (defn requiring-resolve [sym]
    (or (resolve sym)
        (do (locking RT/REQUIRE_LOCK
              (-> sym namespace symbol require))
            (resolve sym))))

  The initial `resolve` is doing two jobs here:
  1. Deciding whether we need to load the file
  2. Resolving a var

  Resolve is not an appropriate function to whether a namespace is loaded using mutable namespaces.

  Let's make this explicit by pulling apart these two jobs, using a placeholder function
  `fully-loaded?` for the step 1 and `resolve` for step 2.

  (defn requiring-resolve [sym]
    (let [lib (-> sym namespace symbol)]
      (or (when (fully-loaded? lib)
            (resolve sym))
          (do (locking RT/REQUIRE_LOCK
                (require lib))
              (resolve sym)))))

  `fully-loaded?` here should return true only if the namespace has completely finished loading. This implementation
  now calls require on one of two conditions:
  1. (not (fully-loaded? lib)), or 
  2. (and (fully-loaded? lib) (not (resolve sym)))

  (defn requiring-resolve [sym]
    (let [lib (-> sym namespace symbol)]
      (or (when (fully-loaded? lib)
            (resolve sym))
          (do (locking RT/REQUIRE_LOCK
                (require lib))
              (resolve sym)))))

  However, the second condition is not needed. We don't need to call require on a lib that is fully loaded since
  it does not change the result of `resolve`. So we can rewrite it as:

  (defn requiring-resolve-sketch [sym]
    (let [lib (-> sym namespace symbol)]
      (when (fully-loaded? lib)
        (locking RT/REQUIRE_LOCK
          (require lib)))
      (resolve sym)))

  The next task is to implement `fully-loaded?`.

  TODO

  The basic approach is to treat the root binding of *loading-libs* differently than thread bindings
  such that it can be used to check if a namespace has finished loading.
 
  It is also unsafe to concurrently call `clojure.core/requiring-resolve` for the same reason as CLJ-2735.


  This is because the function in this namespace treats the root binding of *loading-libs* as only containing
  fully loaded operations, but `clojure.core/requiring-resolve` also adds partially loaded libs. This case
  is not address for performance reasons."
  (:refer-clojure :exclude [requiring-resolve resolve the-ns find-ns remove-ns])
  (:require [clojure.core :as cc]))

(defonce ^:private -removed-ns (atom nil))
(defonce ^:private -global-loaded (.getRawRoot #'cc/*loaded-libs*))

(def ^:private -require-lock
  (let [{:keys [major minor]} *clojure-version*]
    (or (try (.get (.getField clojure.lang.RT "REQUIRE_LOCK") clojure.lang.RT)
             (catch java.lang.NoSuchFieldException _))
        #'clojure.core/require)))

(let [-global-loaded -global-loaded]
  (defn- -loaded? [lib]
    (contains? @-global-loaded lib)))

(let [-global-loaded -global-loaded]
  (defn require [& libs]
    (doseq [lib libs]
      (if (simple-symbol? lib)
        (when-not (-loaded? lib)
          (locking -require-lock
            (when-not (-loaded? lib)
              (let [global-loaded @-global-loaded
                    thread-loaded @@#'cc/*loaded-libs*
                    all-loaded (into global-loaded thread-loaded)
                    loaded (with-bindings {#'cc/*loaded-libs* (ref all-loaded)}
                             (cc/require lib)
                             (apply disj @@#'cc/*loaded-libs* all-loaded))
                    llibs @#'cc/*loaded-libs*
                    llibs-global -global-loaded]
                (dosync
                  (commute llibs into loaded)
                  (when-not (identical? llibs llibs-global)
                    (commute llibs-global into loaded)))))))
        (throw (IllegalArgumentException. (str "Not a simple symbol: " lib)))))))

(let [-removed-ns -removed-ns]
  (defn resolve
    "Resolves namespace-qualified sym only if sym's namespace is fully loaded.
    Thread-safe with simultaneous calls to clojure.core/require only if RT/REQUIRE_LOCK is acquired.
    Not thread-safe with simultaneous calls to clojure.core/requiring-resolve.

    Retries if a remove-ns call occurred between checking if the namespace is loaded
    and resolving the var."
    [sym]
    (if (qualified-symbol? sym)
      (let [lib (-> sym namespace symbol)
            removed @-removed-ns]
        (some-> removed deref)
        (if (-loaded? lib)
          (let [res (cc/resolve sym)]
            (if (identical? removed @-removed-ns)
              res
              (recur sym)))
          (when-not (identical? removed @-removed-ns)
            (recur sym))))
      (throw (IllegalArgumentException. (str "Not a qualified symbol: " sym))))))

(defn requiring-resolve
  "Resolves namespace-qualified sym after ensuring sym's namespace is loaded.
  Thread-safe with simultaneous calls to clojure.core/require only if RT/REQUIRE_LOCK is acquired.
  Not thread-safe with simultaneous calls to clojure.core/requiring-resolve.

  The require call does not respect any wrapping (require :reload-all) calls and will
  only load namespaces that do not appear in the root binding of *loaded-libs*."
  [sym]
  (if (qualified-symbol? sym)
    (do (-> sym namespace symbol require)
        (resolve sym))
    (throw (IllegalArgumentException. (str "Not a qualified symbol: " sym)))))

(let [-removed-ns -removed-ns]
  (defn ns-resolve
    [ns-or-sym sym]
    (let [lib (-> sym namespace symbol)
          removed @-removed-ns]
      (if (-loaded? lib)
        (let [res (cc/resolve sym)]
          (if (identical? removed @-removed-ns)
            res
            (recur ns-or-sym sym)))
        (when-not (identical? removed @-removed-ns)
          (recur ns-or-sym sym))))))

(let [-removed-ns -removed-ns]
  (defn the-ns
    "Returns ns object named by sym only if sym's namespace is fully loaded.

    Throws if namespace was created purely by calling create-ns instead of loading a file or using clojure.core/ns.

    Retry if remove-ns was between checking if the ns was loaded and the the-ns call."
    [ns-or-sym]
    (let [lib (cond-> ns-or-sym
                (not (symbol? ns-or-sym)) ns-name)
          removed @-removed-ns]
      (some-> removed deref)
      (if (-loaded? lib)
        (let [res (if (instance? clojure.lang.Namespace ns-or-sym)
                    ns-or-sym
                    (cc/find-ns ns-or-sym))]
          (if (identical? removed @-removed-ns)
            (or res (throw (Exception. (str "No namespace: " lib " found"))))
            (recur ns-or-sym)))
        (if (identical? removed @-removed-ns)
          (throw (ex-info (str "Namespace not loaded: " lib) {}))
          (recur ns-or-sym))))))

(let [-removed-ns -removed-ns]
  (defn find-ns
    "Returns ns object named by sym only if sym's namespace is fully loaded."
    [sym]
    (let [removed @-removed-ns]
      (if (-loaded? sym)
        (let [res (cc/find-ns sym)]
          (if (identical? removed @-removed-ns)
            res
            (recur sym)))
        (when-not (identical? removed @-removed-ns)
          (recur sym))))))

(let [-global-loaded -global-loaded
      -removed-ns -removed-ns]
  (defn remove-ns [lib]
    @(swap! -removed-ns (fn [prev]
                          (delay
                            (some-> prev deref)
                            (locking -require-lock
                              ;; TODO dissoc from *loaded-libs*. but which op first?
                              (vreset! -removed-ns (java.util.UUID/randomUUID))
                              (dosync (commute -global-loaded disj lib))
                              (cc/remove-ns lib))
                            nil)))
    nil))
