# Next - TBD
- add `areduce` alias for `folda`

# 1.5.1 - 27nd September 2021
- remove API doc for moved `partially-satisifes?`

# 1.5.0 - 27nd September 2021
- add `somef`, `everyp`, `never?`, `run-all!`, `clearing-future{-call}`, `deftest+report-uncaught-contexts, `testing+record-uncaught-contexts`, `folda`

# 1.4.0 - 22nd September 2021
- use simplified but deterministic `find-protocol-impl` from [CLJ-2656](https://clojure.atlassian.net/browse/CLJ-2656)
- document determinism tradeoffs
- [generate API documentation](https://frenchy64.github.io/fully-satisfies/latest)

# 1.2.0 - 17th September 2021
- use Modifier class to decode method modifiers
- only call .getMethods when necessary
- add `partially-satisfies?`

# 1.1.0 - 13th September 2021
- breaking changes: return false for values that don't explicitly extend an :extend-via-metadata protocol with zero methods

# 1.0.2
- walk inheritance chain to find implementation
- fix: nil should not inherit Object implementations
- fix: walk superclass chain even with :extend-via-metadata
- a value always implements an :extend-via-metadata protocol with zero methods

# 1.0.0 - 10th September 2021
- initial public release
