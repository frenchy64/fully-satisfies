<!-- DO NOT EDIT! Instead, edit `dev/resources/root-templates/README.md` and run `./script/regen-selmer.sh` -->
# fully-satisfies

Utility functions for Clojure.

- [fully-satisfies?](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.html#var-fully-satisfies.3F) -- a variant of `clojure.core/satisfies?` that also checks if a value implements all methods in the protocol (considering direct, extended, and metadata methods).
- [partially-satisfies?](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.html#var-partially-satisfies.3F) -- a variant of `clojure.core/satisfies?` that is [compatible with metadata extension](https://clojure.atlassian.net/browse/CLJ-2426).
- [somef](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.somef.html#var-somef) -- a variant of `clojure.core/some-fn` that has a simple operational equivalence, a zero-arity, and [consistent return values](https://clojure.atlassian.net/browse/CLJ-2634).
- [everyp](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.everyp.html#var-everyp) -- a variant of `clojure.core/every-pred` that has a simple operational equivalence, and a zero-arity.
- [never?](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.never.html#var-never.3F) -- a predicate `never?` that always returns false.
- [run-all!](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.run-all.html#var-run-all.21) -- a variant of `clojure.core/run!` that does not [short-circuit on reduced](https://clojure.atlassian.net/browse/CLJ-2574).
- [clearing-future](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.clearing-future.html#var-clearing-future) -- a variant of `clojure.core/future` that clears conveyed bindings after execution, resolving a [known memory leak](https://clojure.atlassian.net/browse/CLJ-2619).
- [deftest](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.uncaught-testing-contexts.html#var-deftest.2Breport-uncaught-contexts), [testing](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.uncaught-testing-contexts.html#var-testing.2Brecord-uncaught-contexts) -- drop-in replacements for `clojure.test/{deftest,testing}` that [report the testing context on uncaught exceptions](https://clojure.atlassian.net/browse/CLJ-2525).
- [folda](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.folda.html#var-folda) -- a variant of `clojure.core/areduce` that supports naming the array.
- [def-shared-protocol](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.shared-protocol.html#var-def-shared-protocol) -- a variant of `clojure.core/defprotocol` whose [methods can see future extensions](https://clojure.atlassian.net/browse/CLJ-1796).
- [vector](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.vector-overflow.html#var-vector), [vec](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.vector-overflow.html#var-vec) -- a vector implementation that [consistently handles integer overflow cases](https://ask.clojure.org/index.php/11080/get-find-assoc-vectors-overflows-key-when-passed-large-longs).
- [latest protocol ops](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.latest-protocol.html) -- implementations of `satisfies?`, `find-protocol-impl`, `find-protocl-method`, `extends?`, `extenders` that look up the latest version of the protocol [such that they have the same behavior with partial](https://clojure.atlassian.net/browse/CLJ-2094).

[Latest API documentation](https://frenchy64.github.io/fully-satisfies/latest)

[Current version API documentation](https://frenchy64.github.io/fully-satisfies/1.5.3)

## Dependency

Available on [Clojars](https://clojars.org/io.github.frenchy64/fully-satisfies).

Leiningen:

```clojure
[io.github.frenchy64/fully-satisfies "1.5.3"]
```

Clojure CLI (Maven deps):

```clojure
  :deps {io.github.frenchy64/fully-satisfies 
         {:mvn/version "1.5.3"}
```

Clojure CLI (git deps):

```clojure
  ;; requires `clj -X:deps prep` to compile java
  :deps {io.github.frenchy64/fully-satisfies 
         {:git/tag "1.5.3", :git/sha "75be470"}}
```

Try it in a REPL:

```clojure
# compile
clj -Sdeps '{:deps {io.github.frenchy64/fully-satisfies {:git/tag "1.5.3", :git/sha "75be470"}}}' -X:deps prep
# start REPL
clj -Sdeps '{:deps {io.github.frenchy64/fully-satisfies {:git/tag "1.5.3", :git/sha "75be470"}}}'
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

Thanks [Wanderson Ferreira](https://github.com/wandersoncferreira) for the idea of `fully-satisfies?` and its name. My initial stance that `fully-satisifes?` was impossible to implement quickly proved to be incorrect after Wanderson's asked the right questions and decompiled some bytecode.

Wanderson and [Mark Herman, II](https://github.com/turbodog99) also helped improve early iterations of `fully-satisfies?`.

## Related work

- https://clojure.atlassian.net/browse/CLJ-2426
- https://clojure.atlassian.net/browse/CLJ-1814
- https://clojure.atlassian.net/browse/CLJ-2656
  - result of making this library and realizing `supers` call is suspicious

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
