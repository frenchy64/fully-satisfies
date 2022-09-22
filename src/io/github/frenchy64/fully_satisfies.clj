(ns io.github.frenchy64.fully-satisfies
  (:refer-clojure :exclude [satisfies?]))

(defn fully-satisfies?
  "Returns true if value v extends protocol p and
  implements every method in protocol p, otherwise false.

  A value is considered to 'extend' protocol p either if:
  - p implements the protocols interface, or
  - p extends the protocol via clojure.core/extend, or
  - p implements at least one method via metadata if supported
    by the protocol
  
  Note that fully-satisfies? aims to be deterministic even
  in cases where protocol dispatch is non-deterministic, and so
  may be innaccurate in those cases.

  In cases of multiple-inheritance of interfaces, fully-satisfies? will pick
  an implementation by sorting the `supers` of the target object by name
  and finding the first interface (from left to right) that implements
  the protocol. Then, if it finds any more-specific interfaces (ie.,
  sub-interfaces) of this interface in the target class hierarchy, it will pick
  that interface's implementation (and apply this algorithm to a fixed point).

  See also:
  - https://clojure.atlassian.net/browse/CLJ-2656
  - https://clojure.atlassian.net/browse/CLJ-1807"
  [p v]
  ((requiring-resolve 'io.github.frenchy64.fully-satisfies.fully-satisfies/fully-satisfies?)
   p v))

(def satisfies? fully-satisfies?)

;; # moved

(defn ^:no-doc partially-satisfies?
  [p v]
  ((requiring-resolve 'io.github.frenchy64.fully-satisfies.partially-satisfies/partially-satisfies?) p v))
