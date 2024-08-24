;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.configurable-core-macros.defmacro
  (:require [clojure.core :as cc]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.defn :as defn]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.utils :as u]))

;;;;;;;;;;;;;;;;
;; defmacro
;;;;;;;;;;;;;;;;

(def info 
  {:dependencies (:dependencies defn/info)
   :sym `defmacro
   :ctor `->defmacro})

;;internal
(defn defmacro-implementation [info name args opts]
  (let [prefix (loop [p (list name) args args]
                 (let [f (first args)]
                   (if (string? f)
                     (recur (cons f p) (next args))
                     (if (map? f)
                       (recur (cons f p) (next args))
                       p))))
        fdecl (loop [fd args]
                (if (string? (first fd))
                  (recur (next fd))
                  (if (map? (first fd))
                    (recur (next fd))
                    fd)))
        fdecl (if (vector? (first fdecl))
                (list fdecl)
                fdecl)
        add-implicit-args (fn [fd]
                            (let [args (first fd)]
                              (cons (vec (cons '&form (cons '&env args))) (next fd))))
        add-args (fn [acc ds]
                   (if (nil? ds)
                     acc
                     (let [d (first ds)]
                       (if (map? d)
                         (conj acc d)
                         (recur (conj acc (add-implicit-args d)) (next ds))))))
        fdecl (seq (add-args [] fdecl))
        decl (loop [p prefix d fdecl]
               (if p
                 (recur (next p) (cons (first p) d))
                 d))]
    (list 'do
          (defn/defn-implementation info (first decl) (rest decl) true opts)
          (list '. (list 'var name) '(setMacro))
          (list 'var name))))

(defmacro ->defmacro [opts]
  (let [macro-name (u/rename-to `defmacro opts)]
    `(do
       (def
         ~(with-meta macro-name
                     {:doc "Like defn, but the resulting function name is declared as a
                           macro and will be used as a macro by the compiler when it is
                           called."
                      :arglists ''([name doc-string? attr-map? [params*] body]
                                   [name doc-string? attr-map? ([params*] body)+ attr-map?])
                      ;:added "1.0"
                      })
         (fn [&form# &env# name# & args#]
           (defmacro-implementation info name# args# '~opts)))
       (doto (var ~macro-name) .setMacro))))
