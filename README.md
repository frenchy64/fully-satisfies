<!-- DO NOT EDIT! Instead, edit `dev/resources/root-templates/README.md` and run `./script/regen-selmer.sh` -->
# fully-satisfies

Utilities for Clojure.

- [fully-satisfies?](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.html#var-fully-satisfies.3F) -- a variant of `clojure.core/satisfies?` that also checks if a value implements all methods in the protocol (considering direct, extended, and metadata methods).
- [partially-satisfies?](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.partially-satisfies.html#var-partially-satisfies.3F) -- a variant of `clojure.core/satisfies?` that is [compatible with metadata extension](https://clojure.atlassian.net/browse/CLJ-2426).
- [somef](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.somef.html#var-somef) -- a variant of `clojure.core/some-fn` that has a simple definitional equivalence, a zero-arity, and [consistent return values](https://clojure.atlassian.net/browse/CLJ-2634).
- [everyp](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.everyp.html#var-everyp) -- a variant of `clojure.core/every-pred` that has a simple definitional equivalence, and a zero-arity.
- [never?](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.never.html#var-never.3F) -- a predicate `never?` that always returns false.
- [run-all!](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.run-all.html#var-run-all.21) -- a variant of `clojure.core/run!` that does not [short-circuit on reduced](https://clojure.atlassian.net/browse/CLJ-2574).
- [clearing-future](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.clearing-future.html#var-clearing-future) -- a variant of `clojure.core/future` that clears conveyed bindings after execution, resolving a [known memory leak](https://clojure.atlassian.net/browse/CLJ-2619).
- [deftest](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.uncaught-testing-contexts.html#var-deftest.2Breport-uncaught-contexts), [testing](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.uncaught-testing-contexts.html#var-testing.2Brecord-uncaught-contexts) -- drop-in replacements for `clojure.test/{deftest,testing}` that [report the testing context on uncaught exceptions](https://clojure.atlassian.net/browse/CLJ-2525).
- [folda](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.folda.html#var-folda) -- a variant of `clojure.core/areduce` that [supports naming the array](https://clojure.atlassian.net/browse/CLJ-115).
- [def-shared-protocol](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.shared-protocol.html#var-def-shared-protocol) -- a variant of `clojure.core/defprotocol` whose [methods can see future extensions](https://clojure.atlassian.net/browse/CLJ-1796).
- non-overflowing [vector](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.vector-overflow.html#var-vector), [vec](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.vector-overflow.html#var-vec) -- a vector implementation that [consistently handles integer overflow cases](https://ask.clojure.org/index.php/11080/get-find-assoc-vectors-overflows-key-when-passed-large-longs), resolving [a known general attack vector](https://ask.clojure.org/index.php/11080/get-find-assoc-vectors-overflows-key-when-passed-large-longs?show=11084#a11084) caused by [undefined behavior](https://ask.clojure.org/index.php/11080/get-find-assoc-vectors-overflows-key-when-passed-large-longs?show=11081#a11081).
- [latest protocol ops](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.latest-protocol.html) -- implementations of `satisfies?`, `find-protocol-impl`, `find-protocol-method`, `extends?`, `extenders` that look up the latest version of the protocol [such that they have the same behavior with partial](https://clojure.atlassian.net/browse/CLJ-2094).
- non-leaky [clojure.core](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.non-leaky-macros.clojure.core.html), [clojure.test](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.non-leaky-macros.clojure.test.html) macros -- versions of the following macros that don't leak implementation details (eg., recur targets, pre/post, try/catch syntax): `locking` ([upstream report](https://clojure.atlassian.net/browse/CLJ-2573)), `binding`, `with-bindings`, `sync`, `with-local-vars`, `with-in-str`, `dosync`, `with-precision`, `with-loading-context`, `with-redefs`, `delay`, `vswap!`, `lazy-seq`, `lazy-cat`, `future`, `pvalues`, `clojure.test/{deftest,deftest-,testing,with-test,with-test-out}`, `clojure.java.shell/with-sh-{dir,env}`, `clojure.test.tap/with-tap-output`, `clojure.pprint/with-pprint-dispatch`, `clojure.core.async/thread`, `clojure.core.logic.pldb/with-{db,dbs}`, `clojure.tools.trace/dotrace`, `clojure.test.check.properties/for-all`, `clojure.test.check.generators/let`, `clojure.java.jmx/with-connection`, `clojure.core.match.debug/with-recur`
- [`::reify-args`](https://github.com/frenchy64/fully-satisfies/blob/main/src/io/github/frenchy64/fully_satisfies/reify_spec.clj), a [spec for clojure.core/reify](https://clojure.atlassian.net/browse/CLJ-2661). Use [register-reify-spec](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.reify-spec.html#var-register-reify-spec) to add to spec's registry.
- [Leaky-seq detection testing framework](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.leaky-seq-detection.html) for detecting memory leaks caused by holding onto the head of sequences. An [example test suite](https://github.com/frenchy64/fully-satisfies/blob/main/test/io/github/frenchy64/fully_satisfies/leaky_seq_detection_test.clj) is provided exposing issues with clojure.core functions and verifying fixes to them.
- [Safer](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.safer.html) variants of clojure.core functions that improve thread-safety and general robustness when passed mutating collections. Includes: `butlast`, `drop-last`, `every?`, `last`, `not-every?`, `nthrest`, `partitionv-all`, `sort`, `sort-by`, `split-at`, `split-with`, `splitv-at`, `take-last`. Includes a [test suite](https://github.com/frenchy64/fully-satisfies/blob/main/test/io/github/frenchy64/fully_satisfies/safer_test.clj) demonstrating the differences and improvements.
- [Lazier](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.lazier.html) variants of clojure.core functions that are slightly lazier when processing and/or returning lazy seqs. Includes: `bounded-count`, `cycle`, `dedupe`, `iterator-seq`, `sequence`. Includes a [test suite](https://github.com/frenchy64/fully-satisfies/blob/main/test/io/github/frenchy64/fully_satisfies/lazier_test.clj) demonstrating the differences and improvements.
- [Uniformly generalized](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.uniform.html) variants of clojure.core functions such that they work consistently for all values. Includes: `dedupe`, `halt-when`, `partition-by`. Includes a [test suite](https://github.com/frenchy64/fully-satisfies/blob/main/test/io/github/frenchy64/fully_satisfies/uniform_test.clj) demonstrating the differences and improvements.
- [Head-releasing](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.head-releasing.html) variants of clojure.core functions that release the head of seqs earlier have thus have improved memory usage characteristics. Includes: `every?`, `keep`, `keep-indexed`, `map`, `map-indexed`, `mapcat`, `naive-seq-reduce`, `not-any?`, `not-every?`, `some`. Includes a [test suite](https://github.com/frenchy64/fully-satisfies/blob/main/test/io/github/frenchy64/fully_satisfies/leaky_seq_detection_test.clj) demonstrating the differences and improvements, using the [leaky-seq detection testing framework](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.leaky-seq-detection.html).


[Latest API documentation](https://frenchy64.github.io/fully-satisfies/latest)

[Current version API documentation](https://frenchy64.github.io/fully-satisfies/1.10.0)

## Dependency

Available on [Clojars](https://clojars.org/io.github.frenchy64/fully-satisfies).

Leiningen:

```clojure
[io.github.frenchy64/fully-satisfies "1.10.0"]
```

Clojure CLI (Maven deps):

```clojure
  :deps {io.github.frenchy64/fully-satisfies 
         {:mvn/version "1.10.0"}}
```

Clojure CLI (git deps):

```clojure
  ;; requires `clj -X:deps prep` to compile java
  :deps {io.github.frenchy64/fully-satisfies 
         {:git/tag "1.10.0", :git/sha "8a4d35a"}}
```

Try it in a REPL:

```clojure
# compile
clj -Sdeps '{:deps {io.github.frenchy64/fully-satisfies {:git/tag "1.10.0", :git/sha "8a4d35a"}}}' -X:deps prep
# start REPL
clj -Sdeps '{:deps {io.github.frenchy64/fully-satisfies {:git/tag "1.10.0", :git/sha "8a4d35a"}}}'
```

## Usage

### fully-satisfies?

[Docstring](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.html#var-fully-satisfies.3F)

```clojure
(require '[io.github.frenchy64.fully-satisfies :refer [fully-satisfies?]])

(defprotocol A
  (a [this])
  (b [this]))

(fully-satisfies? A (reify))
;=> false
(fully-satisfies? A (reify A))
;=> false
(fully-satisfies? A (reify A (a [this])))
;=> false
(fully-satisfies? A (reify A (a [this]) (b [this])))
;=> true
```

### run-all!

```clojure
(require '[io.github.frenchy64.fully-satisfies.run-all :refer [run-all!]])

(run-all! println [1 (reduced 2) 3])
;1
;2
;3
;=> nil
;; does not short-circuit on reduced
(run-all! println [1 (reduced 2) 3])
;1
;#object[clojure.lang.Reduced 0x3deeac1 {:status :ready, :val 2}]
;3
;=> nil
```

## Releasing

1. Change project.clj version to desired version + SNAPSHOT
   - eg., `1.0.0-SNAPSHOT`
2. Commit with message `Release {:major,:minor,:patch}`
   - this releases the current version then bumps to the next `{:major,:minor,:patch}` SNAPSHOT
3. Pull

## Thanks

Thanks [Wanderson Ferreira](https://github.com/wandersoncferreira) for the idea of `fully-satisfies?` and its name. My initial stance that `fully-satisfies?` was impossible to implement quickly proved to be incorrect after Wanderson's asked the right questions and decompiled some bytecode.

Wanderson and [Mark Herman, II](https://github.com/turbodog99) also helped improve early iterations of `fully-satisfies?`.

## Related work

- https://clojure.atlassian.net/browse/CLJ-2426
- https://clojure.atlassian.net/browse/CLJ-1814
- https://clojure.atlassian.net/browse/CLJ-2656
  - result of making this library and realizing `supers` call is suspicious

## TODO

- https://clojure.atlassian.net/browse/CLJ-2162
- https://clojure.atlassian.net/browse/CLJ-2069
- agents memory leak via conveyed bindings
- https://ask.clojure.org/index.php/11080/get-find-assoc-vectors-overflows-key-when-passed-large-longs?show=11137#c11137
```
Clojure 1.10.3
user=> (get (into-array [1 2 42]) 4294967296)
1
user=> (get "123" 4294967296)
\1
user=> (get (into-array [1 2 42]) 4294967296 :not-found)
1
user=> (get "123" 4294967296 :not-found)
\1
```
- https://clojure.atlassian.net/browse/CLJ-2322
- `throwArity(21)` in AFn.java should be `throwArity(20+args.length)`
  - reprod:
```
Clojure 1.11.1
user=> (apply {} (range 21))
Execution error (ArityException) at user/eval214 (REPL:1).
Wrong number of args (21) passed to: clojure.lang.PersistentArrayMap
user=> (apply {} (range 22))
Execution error (ArityException) at user/eval216 (REPL:1).
Wrong number of args (21) passed to: clojure.lang.PersistentArrayMap
user=> (apply {} (range 24))
Execution error (ArityException) at user/eval218 (REPL:1).
Wrong number of args (21) passed to: clojure.lang.PersistentArrayMap
```
- same in RestFn.java
  - reprod:
```
Clojure 1.11.1
user=> (defmacro big-fn [nargs] `(fn ~(conj (mapv #(gensym (do % "arg")) (range nargs)) 'last)))
#'user/big-fn
user=> (apply (big-fn 19) (range 21))
Execution error (ArityException) at user/eval145 (REPL:1).
Wrong number of args (21) passed to: user/eval145/fn--165
user=> (apply (big-fn 19) (range 22))
Execution error (ArityException) at user/eval168 (REPL:1).
Wrong number of args (21) passed to: user/eval168/fn--188
user=> (apply (big-fn 19) (range 24))
Execution error (ArityException) at user/eval191 (REPL:1).
Wrong number of args (21) passed to: user/eval191/fn--211
```

- direct-linkable protocols
  - apparently a matter of makings protocols non-closures
  - https://ask.clojure.org/index.php/10967/are-protocol-methods-guaranteed-to-not-be-directly-linked?show=10990#a10990
  - approach:
    - add new field to clojure.lang.MethodImplCache$Entry which distinguishes between 
      the `(.isInstance c x)` test in `-cache-protocol-fn`
    - update the `identical?` clause in the with-meta-extension part to grab this
      entry raw (eg., replace the `fnFor` with `fnEntryFor`)
    - move ginterf into the -cache-protocol-fn calls (1 arity at a time)

- defmulti with cache controls https://clojure.atlassian.net/browse/CLJ-2626

## License

Where noted, contains code from Clojure under license:

```
Copyright (c) Rich Hickey. All rights reserved.
The use and distribution terms for this software are covered by the
Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
which can be found in the file epl-v10.html at the root of this distribution.
By using this software in any fashion, you are agreeing to be bound by
the terms of this license.
You must not remove this notice, or any other, from this software.
```

Otherwise:

```
Copyright © 2021 Ambrose Bonnaire-Sergeant

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
```
