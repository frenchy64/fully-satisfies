;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.configurable-core-macros.defn
  (:require [clojure.core :as cc]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.utils :as u]))

;;;;;;;;;;;;;;;;
;; defn
;;;;;;;;;;;;;;;;

(def info {`let {:dependencies #{`fn}}})

(defn- visit-first-sig-param [fdecl f]
  (let [argv (fn [sig]
               (with-meta
                 (cond-> sig
                   (and (vector? sig)
                        (pos? (count sig)))
                   (update 0 (fn [s] (with-meta (f s) (meta s)))))
                 (meta sig)))]
    (with-meta
      (map (fn [sigs]
             (if (vector? sigs)
               (argv sigs)
               (with-meta (map argv sigs) (meta sigs))))
           fdecl)
      (meta fdecl))))

(defn- sigs
  "Massages fdecl before calling clojure.core/sigs to work
  around https://clojure.atlassian.net/browse/CLJ-2874"
  [fdecl macro?]
  (#'cc/assert-valid-fdecl fdecl)
  (let [gform (gensym '&form)
        s (#'cc/sigs (cond-> fdecl
                       (not macro?)
                       (visit-first-sig-param (fn [s]
                                                (if (= '&form s)
                                                  gform
                                                  s)))))]
    (cond-> s
      (not macro?) (visit-first-sig-param (fn [s]
                                            (if (= gform s)
                                              '&form
                                              s))))))

;;internal
(defn defn-implementation [&form &env name fdecl opts]
  (if (instance? clojure.lang.Symbol name)
    nil
    (throw (IllegalArgumentException. "First argument to defn must be a symbol")))
  (let [m (if (string? (first fdecl))
            {:doc (first fdecl)}
            {})
        fdecl (if (string? (first fdecl))
                (next fdecl)
                fdecl)
        m (if (map? (first fdecl))
            (conj m (first fdecl))
            m)
        fdecl (if (map? (first fdecl))
                (next fdecl)
                fdecl)
        fdecl (if (vector? (first fdecl))
                (list fdecl)
                fdecl)
        m (if (map? (last fdecl))
            (conj m (last fdecl))
            m)
        fdecl (if (map? (last fdecl))
                (butlast fdecl)
                fdecl)
        m (conj {:arglists (list 'quote (sigs fdecl false))} m)
        m (let [inline (:inline m)
                ifn (first inline)
                iname (second inline)]
            ;; same as: (if (and (= 'fn ifn) (not (symbol? iname))) ...)
            (if (if (clojure.lang.Util/equiv 'fn ifn)
                  (if (instance? clojure.lang.Symbol iname) false true))
              ;; inserts the same fn name to the inline fn if it does not have one
              (assoc m :inline (cons ifn (cons (clojure.lang.Symbol/intern (.concat (.getName ^clojure.lang.Symbol name) "__inliner"))
                                               (next inline))))
              m))
        m (conj (if (meta name) (meta name) {}) m)]
    (list 'def (with-meta name m)
          ;;todo - restore propagation of fn name
          ;;must figure out how to convey primitive hints to self calls first
          ;;(cons `fn fdecl)
          (with-meta (cons (u/replacement-for `fn opts) fdecl) {:rettag (:tag m)}))))

(defmacro ->defn [opts]
  (let [macro-name (u/rename-to `defn opts)]
    `(do (def
           ^{:doc "Same as (def name (fn [params* ] exprs*)) or (def
                  name (fn ([params* ] exprs*)+)) with any doc-string or attrs added
                  to the var metadata. prepost-map defines a map with optional keys
                  :pre and :post that contain collections of pre or post conditions."
             :arglists '~'([name doc-string? attr-map? [params*] prepost-map? body]
                           [name doc-string? attr-map? ([params*] prepost-map? body)+ attr-map?])
             ;:added "1.0"
             }
           ~macro-name (fn ~macro-name [&form# &env# name# & fdecl#]
                         (defn-implementation &form# &env# name# fdecl# '~opts)))
         (doto (var ~macro-name) .setMacro))))
