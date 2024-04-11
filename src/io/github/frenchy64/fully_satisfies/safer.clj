(ns io.github.frenchy64.fully-satisfies.safer
  (:refer-clojure :exclude [butlast every? take-last]))

;;TODO unit test
(defn every?
  "Returns true if (pred x) is logical true for every x in coll, else
  false."
  {:tag Boolean
   :added "1.0"
   :static true}
  [pred coll]
  ;; reuses result of `seq` - Ambrose
  (if-let [coll (seq coll)]
    (if (pred (first coll))
      (recur pred (next coll))
      false)
    true))

(defn take-last
  "Returns a seq of the last n items in coll.  Depending on the type
  of coll may be no better than linear time.  For vectors, see also subvec."
  {:added "1.1"
   :static true}
  [n coll]
  (let [s (seq coll)] ;; pull seq call before loop initialization - Ambrose
    (loop [s s, lead (seq (drop n s))]
      (if lead
        (recur (next s) (next lead))
        s))))
