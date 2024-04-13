# NEXT - TBD

# 1.10.1-SNAPSHOT - April 13th 2024
- improve documentation and methodology for code in 1.10.0
- add `io.github.frenchy64.fully-satisfies.leaky-seq-detection.is-strong.false-positive-detection=true` Java property to try harder to catch false-positive strong references.
- alias `dedupe` in `io.github.frenchy64.fully-satisfies.uniform` since it is both lazier and behaves more uniformly.

# 1.10.0 - April 12th 2024
- [Leaky-seq detection testing framework](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.leaky-seq-detection.html) for detecting memory leaks caused by holding onto the head of sequences. An [example test suite](https://github.com/frenchy64/fully-satisfies/blob/main/test/io/github/frenchy64/fully_satisfies/leaky_seq_detection_test.clj) is provided exposing issues with clojure.core functions and verifying fixes to them.
- [Safer](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.safer.html) variants of clojure.core functions that improve thread-safety and general robustness when passed mutating collections. Includes: `butlast`, `drop-last`, `every?`, `last`, `not-every?`, `nthrest`, `partitionv-all`, `sort`, `sort-by`, `split-at`, `split-with`, `splitv-at`, `take-last`. Includes a [test suite](https://github.com/frenchy64/fully-satisfies/blob/main/test/io/github/frenchy64/fully_satisfies/safer_test.clj) demonstrating the differences and improvements.
- [Lazier](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.lazier.html) variants of clojure.core functions that are slightly lazier when processing and/or returning lazy seqs. Includes: `bounded-count`, `cycle`, `dedupe`, `iterator-seq`, `sequence`. Includes a [test suite](https://github.com/frenchy64/fully-satisfies/blob/main/test/io/github/frenchy64/fully_satisfies/lazier_test.clj) demonstrating the differences and improvements.
- [Uniformly generalized](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.uniform.html) variants of clojure.core functions such that they work consistently for all values. Includes: `partition-by`, `halt-when`. Includes a [test suite](https://github.com/frenchy64/fully-satisfies/blob/main/test/io/github/frenchy64/fully_satisfies/uniform_test.clj) demonstrating the differences and improvements.
- [Head-releasing](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.head-releasing.html) variants of clojure.core functions that release the head of seqs earlier have thus have improved memory usage characteristics. Includes: `every?`, `keep`, `keep-indexed`, `map`, `map-indexed`, `mapcat`, `naive-seq-reduce`, `not-any?`, `not-every?`, `some`. Includes a [test suite](https://github.com/frenchy64/fully-satisfies/blob/main/test/io/github/frenchy64/fully_satisfies/leaky_seq_detection_test.clj) demonstrating the differences and improvements, using the [leaky-seq detection testing framework](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.leaky-seq-detection.html).

# 1.9.0 - 27th September 2022
- add spec for `reify` arguments: `:io.github.frenchy64.fully-satisfies.reify-spec/reify-args`
- fix `io.github.frenchy64.fully-satisfies.non-leaky-macros.clojure.tools.trace/non-leaky-dotrace`

# 1.7.1 - 22nd September 2022
- https://github.com/frenchy64/fully-satisfies/issues/6 fix `everyp`/`somef` short-circuiting order
- improve `io.github.frenchy64.fully-satisfies.uncaught-testing-contexts`
  - use atom to track exceptional contexts for binding conveyance compatibility
  - remove heuristics for choosing exceptional context. first wins for (root-cause of) each exception.
  - report most likely exceptional context first, then list other candidates.

# 1.7.0 - 30th September 2021
- add `non-leaky-macros` namespaces

# 1.6.0 - 27nd September 2021
- add `def-shared-protocol`
- add `vector-overflow` namespace
- add `latest-protocol` namespace

# 1.5.2 - 27nd September 2021
- add `areduce` alias for `folda`

# 1.5.1 - 27nd September 2021
- remove API doc for moved `partially-satisfies?`

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
