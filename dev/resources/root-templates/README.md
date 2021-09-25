{% do-not-edit-xml-comment %}
# fully-satisfies

Utility functions for Clojure.

- [fully-satisfies?](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.html#var-fully-satisfies.3F) -- a variant of `clojure.core/satisfies?` that also checks if a value implements all methods in the protocol.
- [partially-satisfies?](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.html#var-partially-satisfies.3F) -- a variant of `clojure.core/satisfies?` that is compatible with metadata extension.
- _somef_ -- a variant of `clojure.core/some-fn` that has a simple operational equivalence.
- _everyp_ -- a variant of `clojure.core/every-pred` that has a simple operational equivalence.
- _never?_ -- a predicate `never?` that always returns false.
- _each_ -- a variant of `clojure.core/run!` that does not short-circuit on reduced.

[Latest API documentation](https://frenchy64.github.io/fully-satisfies/latest)

[Current version API documentation](https://frenchy64.github.io/fully-satisfies/{◊current-version◊})

## Dependency

Available on [Clojars](https://clojars.org/io.github.frenchy64/fully-satisfies).

Leiningen:

```clojure
[io.github.frenchy64/fully-satisfies "{◊current-version◊}"]
```

Clojure CLI:

```clojure
  :deps {io.github.frenchy64/fully-satisfies 
         {:git/tag "{◊current-version◊}", :git/sha "{◊short-sha◊}"}}
```

Try it in a REPL:

```clojure
clj -Sdeps \
    '{:deps 
      {io.github.frenchy64/fully-satisfies 
       {:git/tag "{◊current-version◊}", :git/sha "{◊short-sha◊}"}}}'
;; See usage below for how to proceed
```

## Usage

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
