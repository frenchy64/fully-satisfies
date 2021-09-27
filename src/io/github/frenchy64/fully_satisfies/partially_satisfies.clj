(ns io.github.frenchy64.fully-satisfies.partially-satisfies
  (:refer-clojure :exclude [satisfies?])
  (:import [java.lang.reflect Method Modifier]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;;TODO can we use a cached find-protocol-impl?
;; https://clojure.atlassian.net/browse/CLJ-1814

;; from https://clojure.atlassian.net/browse/CLJ-2426
(defn partially-satisfies?
  "Returns true if value v extends protocol p, otherwise false.

  A value is considered to 'extend' protocol p either if:
  - p implements the protocols interface, or
  - p extends the protocol via clojure.core/extend, or
  - p implements at least one method via metadata if supported
    by the protocol"
  [p v]
  (boolean
    (or (find-protocol-impl p v)
        (when (:extend-via-metadata p)
          (when-some [vm (not-empty (meta v))]
            (when-some [method-map-keys (-> p :method-map keys seq)]
              (let [nstr (-> p :var symbol namespace)]
                (some (fn [mmap-key]
                        (get vm (symbol nstr (name mmap-key))))
                      method-map-keys))))))))

(def satisfies? partially-satisfies?)
