(ns io.github.frenchy64.fully-satisfies.safer
  (:refer-clojure :exclude [butlast every? split-at split-with take-last nthrest])
  (:require [io.github.frenchy64.fully-satisfies.lazier :as lazier]))

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

(defn drop-last
  "Return a lazy sequence of all but the last n (default 1) items in coll"
  {:added "1.0"
   :static true}
  ([coll] (drop-last 1 coll))
  ([n coll] (let [coll (lazier/sequence coll)] ;; bind a sequence - Ambrose
              (map (fn [x _] x) coll (lazier/drop n coll)))))

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

(defn split-at
  "Returns a vector of [(take n coll) (drop n coll)]"
  {:added "1.0"
   :static true}
  [n coll]
  (let [coll (seq coll)] ;; call seq on `coll` - Ambrose
    [(take n coll) (drop n coll)]))

(defn split-with
  "Returns a vector of [(take-while pred coll) (drop-while pred coll)]"
  {:added "1.0"
   :static true}
  [pred coll]
  (let [coll (seq coll)] ;; call seq on `coll` - Ambrose
    [(take-while pred coll) (drop-while pred coll)]))

(when-not (= "true" (System/getProperty "io.github.frenchy64.fully-satisfies.safer.drop.no-1.12-perf-warn"))
  (when (try (Class/forName "clojure.lang.IDrop")
             (catch Throwable _))
    (println "WARNING: io.github.frenchy64.fully-satisfies.safer/nthrest is missing 1.12 performance features")))

(defn nthrest
  "Returns the nth rest of coll, coll when n is 0."
  {:added "1.3"
   :static true}
  [coll n]
  (do #_if #_(instance? clojure.lang.IDrop coll)
    #_
    (if (pos? n)
      (or (.drop ^clojure.lang.IDrop coll (if (int? n) n (Math/ceil n))) ())
      coll)
    (loop [n n xs coll]
      (if-let [xs (and (pos? n) (seq xs))]
        (recur (dec n) (rest xs))
        xs))))
