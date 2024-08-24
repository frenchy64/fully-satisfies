(ns io.github.frenchy64.fully-satisfies.configurable-core-macros.utils)

(defn resolve-opts [opts]
  {:post [(map? %)]}
  (if (map? opts)
    opts
    (let [s (if (symbol? opts)
              opts
              (let [_ (assert (= 2 (count opts)))
                    [q s] opts]
                (assert (= 'quote (first opts)))
                s))]
      (assert (qualified-symbol? s))
      (assoc @(resolve s) :opts-var s))))

;; TODO pass opts to result somehow
(defn replacement-for [info vsym opts]
  (assert (or (var? vsym)
              (qualified-symbol? vsym)))
  (let [opts (resolve-opts opts)
        v? (var? vsym)
        ;; stay compatible with 1.9 for now by 
        vsym (if (var? vsym)
               (symbol (some-> (.ns ^clojure.lang.Var vsym) ns-name name) (.name (.sym ^clojure.lang.Var vsym)))
               (symbol vsym))
        _ (assert (get (:dependencies info) vsym)
                  (str "Must declare dependency on " vsym
                       " for " (:ctor info)))
        vr (or (get (:replace opts) vsym)
               (get (:rename opts) vsym)
               vsym)]
    (assert (qualified-symbol? vr))
    (cond-> vr
      v? find-var)))

(defn rename-to [vsym opts]
  {:pre [(qualified-symbol? vsym)]
   :post [(simple-symbol? %)]}
  (let [qsym (get (:rename (resolve-opts opts)) vsym vsym)]
    (assert (qualified-symbol? qsym) qsym)
    (-> qsym name symbol)))

(defn define? [sym opts]
  {:pre [(qualified-symbol? sym)]}
  (let [{:keys [exclude replace]} (resolve-opts opts)]
    (and (not (contains? exclude sym))
         (not (contains? replace sym)))))
