(ns io.github.frenchy64.fully-satisfies.exponential-explosion
  (:require [clojure.spec.alpha :as s]))

;; idea: cache every sublist in a macroexpand-1 argument's call by using s/macroexpand-check
;; in a soft and/or ttl cache. if this list shows up twice by pointer identity as a target
;; of a macroexpand-1 call, then it has probably been duplicated by a macro.
;; will not work for special forms or non-macro calls due to Compiler.java hardcoding
;; the conditions of calling s/macroexpand-check. probably still useful since exponential
;; code blowup is most critical when you duplicate macro calls.

;; keep a history of expansions that contain the same list so an expansion trace can be provided.

;; (doseq [a nil] (go a)) ;; cache form '(go a) by pointer identity
;; vv expand
;; (if (chunked? s) (go a) (go a)) ;; nothing we can do, special form
;; vv expand then
;; (go a)   ;; increment cache '(go a)
;; vv expand else
;; (go a)   ;; increment cache '(go a) -- error! found twice.

;(defonce )

(defn monkey-patch-macroexpand-check! []
  (alter-var-root #'s/macroexpand-check
                  (fn [macroexpand-check]
                    (fn [v args]
                      (macroexpand-check v args)))))

(defonce __monkey-patch__
  (when-not (= "true" (System/getProperty "io.github.frenchy64.fully-satisfies.exponential-explosion.no-monkeypatch"))
    (monkey-patch-macroexpand-check!)))
