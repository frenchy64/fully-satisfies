(ns io.github.frenchy64.fully-satisfies.exponential-explosion
  (:refer-clojure :exclude [doseq])
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.pprint :as pp]
            [io.github.frenchy64.fully-satisfies.linear-expansion :refer [doseq]]))

;;FIXME this idea won't work because s/macroexpand-check doesn't take the full
;; form, it takes the macro Var and a list of arguments. might need to index by
;; the qualified or unqualified macro symbol.

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

(defmacro ^:private dbg [f] `(let [v# ~f] (prn '~f :=> v#) v#))

(def ^:dynamic *state* nil)

(defn identical-vec-contents? [args args']
  {:pre [(vector? args)
         (vector? args')]}
  (and (= args' args)
       (reduce (fn [_ i]
                 (if (identical? (nth args i) (nth args' i))
                   true
                   (reduced false)))
               true (range (count args)))))

(defn id->form [{:keys [msym args]}] (cons (-> msym name symbol) args))

(defn lint-macro-call [v args]
  ;(prn [v args])
  (when-some [state *state*]
    (let [;; TODO coerce to edn (remove vars)
          args (vec args)
          info {:file *file*
                :line (.deref clojure.lang.Compiler/LINE)
                :column (.deref clojure.lang.Compiler/COLUMN)}
          msym (symbol v)
          id {:msym msym :args args :info info}
          nargs (count args)]
      (swap! state
             (fn [{:keys [history] :as m}]
               (let [seen? (some #(identical-vec-contents? args (:args %)) (get-in m [:seen id]))
                     trace (when seen?
                             ;; go back and find the parent macro for this form.
                             (let [vname (name msym)
                                   likely-macro-call? #(and (symbol? %)
                                                            (= vname (name %)))]
                               (when-let [parent-idx (some (fn [i]
                                                             (let [{pargs :args :as candidate-parent} (nth history i)
                                                                   candidates (volatile! [])
                                                                   _ (run! (fn [form]
                                                                             ;; maybe prewalk and skip quoted things?
                                                                             (walk/postwalk (fn [v]
                                                                                              (when (and (seq? v)
                                                                                                         (likely-macro-call? (first v))
                                                                                                         (= nargs (bounded-count (+ 2 nargs) (next v)))
                                                                                                         (identical-vec-contents? args (vec (next v))))
                                                                                                (vswap! candidates conj v))
                                                                                              v)
                                                                                            form)
                                                                             nil)
                                                                           pargs)]
                                                               (when (seq @candidates)
                                                                 i)))
                                                           (range (count history))
                                                           #_(range (dec (count history)) -1 -1))]
                                 ;; TODO only warn if this form has been duplicated between now and the parent
                                 (let [trace (subvec history parent-idx)]
                                   (when (some (fn [{msym' :msym args' :args info' :info}]
                                                 (and (= msym msym')
                                                      (= info info')
                                                      (identical-vec-contents? args args')))
                                               trace)
                                     trace)))))]
                 (-> m
                     (update :history (fnil conj []) id)
                     (cond->
                       (not seen?)
                       (update-in [:seen id] (fnil conj []) id)

                       trace (update :suspects (fnil conj [])
                                     {:id id :trace trace})))))))))

(defn report-issues [state]
  (doseq [{:keys [id trace] :as suspect} (:suspects state)
          :let [{:keys [file line column]} (:info id)
                form (id->form id)
                parent-form (id->form (first trace))
                nduplicates (inc (get (frequencies trace) id))]]
    (println (str (peek (str/split file #"/")) ":" line ":" column) "Form" (pr-str form) "expanded" 
             nduplicates "times"; "\n"
             #_(str/join " " (interpose :=> (mapv id->form trace))))))

(defn patched-macroexpand-check [macroexpand-check v args]
  (lint-macro-call v args)
  (macroexpand-check v args))

(defn monkey-patch-macroexpand-check! []
  (alter-var-root #'s/macroexpand-check
                  (fn [macroexpand-check]
                    (fn [v args]
                      (patched-macroexpand-check macroexpand-check v args)))))

(defn patched-load [load args]
  (doseq [arg args]
    ;(prn "patched-load" arg)
    (binding [*state* (atom {})]
      (load arg)
      ;(pp/pprint @*state*)
      (report-issues @*state*))))

(defn monkey-patch-load! []
  (alter-var-root #'load
                  (fn [load]
                    (fn [& args]
                      (patched-load load args)))))

(defn patched-eval [eval form]
  (binding [*state* (atom {})]
    (let [res (eval form)] 
      (report-issues @*state*)
      ;(pp/pprint @*state*)
      res)))

(defn monkey-patch-eval! []
  (alter-var-root #'eval
                  (fn [eval]
                    (fn [form]
                      (patched-eval eval form)))))

(comment
  (require (ns-name *ns*) :reload)
  (defonce __monkey-patch__
    (do (monkey-patch-load!)
        (monkey-patch-eval!)
        (monkey-patch-macroexpand-check!)
        ))
  )

#_
(defonce __monkey-patch__
  (when-not (= "true" (System/getProperty "io.github.frenchy64.fully-satisfies.exponential-explosion.no-monkeypatch"))
    (monkey-patch-macroexpand-check!)))
