;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.configurable-core-macros.defmethod
  (:require [clojure.core :as cc]
            [io.github.frenchy64.fully-satisfies.configurable-core-macros.utils :as u]))

;;;;;;;;;;;;;;;;
;; defmethod
;;;;;;;;;;;;;;;;

(def info {`defmethod {:dependencies #{`fn}
                       :requires '[io.github.frenchy64.fully-satisfies.configurable-core-macros.defmethod]}})

;;internal
(defn defmethod-implementation [multifn dispatch-val fn-tail opts]
  (assert (symbol? multifn)) ;; pprint strips meta on non-symbols...
  `(. ~(with-meta multifn {:tag 'clojure.lang.MultiFn})
      addMethod
      ~dispatch-val
      (~(u/replacement-for `fn opts) ~@fn-tail)))

(defmacro ->defmethod [opts]
  (let [macro-name (u/rename-to `defmethod opts)]
    `(defmacro ~macro-name
       "Creates and installs a new method of multimethod associated with dispatch-value. "
       {;:added "1.0"
        :arglists '~'([multifn dispatch-val & fn-tail])}
       [multifn# dispatch-val# & fn-tail#]
       (defmethod-implementation multifn# dispatch-val# fn-tail# '~opts))))
