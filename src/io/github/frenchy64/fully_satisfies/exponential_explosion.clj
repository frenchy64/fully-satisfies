(ns io.github.frenchy64.fully-satisfies.exponential-explosion
  (:refer-clojure :exclude [doseq])
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.pprint :as pp]
            [io.github.frenchy64.fully-satisfies.linear-expansion :refer [doseq]]))

;;TODO only consider reexpansions of forms that originate form the current file

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

(defn same-id? [id id']
  (and (= (:msym id) (:msym id'))
       (= (:info id) (:info id'))
       (identical-vec-contents? (:args id) (:args id'))))

(defn lint-macro-call [v args]
  ;(prn [v args])
  (when-some [state *state*]
    (let [;; TODO coerce to edn (remove vars)
          args (vec args)
          file *file*
          info {:file file
                :line (.deref clojure.lang.Compiler/LINE)
                :column (.deref clojure.lang.Compiler/COLUMN)}
          msym (symbol v)
          id {:msym msym :args args :info info}
          nargs (count args)]
      (swap! state
             (fn [{:keys [history] :as m}]
               (if (some-> (:file m) (not= file))
                 m
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
                                     (when (some #(same-id? % id) trace)
                                       trace)))))]
                   (-> m
                       (update :file #(or % file))
                       (update :history (fnil conj []) id)
                       (cond->
                         (not seen?)
                         (update-in [:seen id] (fnil conj []) id)

                         trace (assoc-in [:suspects id] {:trace trace}))))))))))

(defn report-issues [state]
  (doseq [[id {:keys [trace]}] (:suspects state)
          :let [{:keys [file line column]} (:info id)
                form (id->form id)
                parent-form (id->form (first trace))
                nduplicates (inc (get (frequencies trace) id))]]
    (println (str (peek (str/split file #"/")) ":" line ":" column ":") "Expanded" nduplicates "times:" (pr-str form))))

(defn patched-macroexpand-check [macroexpand-check v args]
  ;(prn v args)
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

(defn monkey-patch! []
  (monkey-patch-load!)
  (monkey-patch-eval!)
  (monkey-patch-macroexpand-check!))

(comment
  (require (ns-name *ns*) :reload)
  (defonce __monkey-patch__
    (monkey-patch!))
  )


#_
(defonce __monkey-patch__
  (when-not (= "true" (System/getProperty "io.github.frenchy64.fully-satisfies.exponential-explosion.no-monkeypatch"))
    (monkey-patch-macroexpand-check!)))
