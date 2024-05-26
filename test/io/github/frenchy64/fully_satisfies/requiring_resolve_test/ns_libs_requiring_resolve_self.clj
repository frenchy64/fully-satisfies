;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.requiring-resolve-test.ns-libs-requiring-resolve-self
  (:refer-clojure :exclude [requiring-resolve])
  (:require [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]))

(def a 1)
(assert (= 1 @(requiring-resolve `a)))
