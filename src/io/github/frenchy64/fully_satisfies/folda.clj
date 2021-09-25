;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.github.frenchy64.fully-satisfies.folda
  "Variant of `clojure.core/areduce` that allows the array to be named.")

(defmacro folda
  "Reduces an expression across an array a named (if provided) aname,
  using an index named idx, and return value named ret, initialized to init,
  setting ret to the evaluation of expr at each step, returning ret."
  ([a idx ret init expr]
   `(folda ~(gensym "aname") ~a ~idx ~ret ~init ~expr))
  ([aname a idx ret init expr]
   `(let [a# ~a l# (alength a#)]
      (loop [~idx 0 ~ret ~init]
        (if (< ~idx l#)
          (recur (unchecked-inc-int ~idx) (let [~aname a#] ~expr))
          ~ret)))))
