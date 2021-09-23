(ns io.github.frenchy64.fully-satisfies.dev.selmer
  (:require [selmer.parser :as sp :refer [render render-file]]
            [selmer.util :as su]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh])
  (:import [java.io File]))

(sp/cache-off!)

(defn missing-value-fn [tag context-map]
  (throw (Exception. (str "Missing tag " tag))))

(su/set-missing-value-formatter! missing-value-fn)

(defn do-not-edit-string [src]
  (assert src)
  (format "DO NOT EDIT! Instead, edit `%s` and run `./script/regen-selmer.sh`"
          src))

(sp/add-tag! :do-not-edit-xml-comment
             (fn [args {:keys [src] :as _context-map}]
               (format "<!-- %s -->"
                       (do-not-edit-string src))))

(sp/add-tag! :do-not-edit-edn-comment
             (fn [args {:keys [src] :as _context-map}]
               (assert src)
               (format ";; %s"
                       (do-not-edit-string src))))

(def release-transforms
  "Key is relative to dev/resources/root-templates, val is relative to repo root."
  (into {}
        (map (juxt identity identity))
        #{"README.md"}))

(defn -main []
  (let [opts {:current-version (str/trim (slurp (io/resource "latest-version-tag")))
              :short-sha (str/trim (slurp (io/resource "latest-version-short-sha")))}]
    (doseq [[src dest] release-transforms]
      (spit (str "../" dest)
            (render-file
              (str "root-templates/" src)
              (into opts {;; root-relative
                          :src (str "dev/resources/root-templates/" src)
                          ;; root-relative
                          :dest dest}) 
              {;; {◊var◊} instead of {{var}}
               :filter-open \◊
               :filter-close \◊})))))
