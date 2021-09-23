<!-- DO NOT EDIT! Instead, edit `dev/resources/root-templates/README.md` and run `./script/regen-selmer.sh` -->
# fully-satisfies

Provides a variant of `clojure.core/satisfies?` that also checks if a value implements all methods in the protocol.

[Latest API documentation](https://frenchy64.github.io/fully-satisfies/latest)

[Current version API documentation](https://frenchy64.github.io/fully-satisfies/1.4.0)

## Dependency

Available on [Clojars](https://clojars.org/io.github.frenchy64/fully-satisfies).

Leiningen:

```clojure
[io.github.frenchy64/fully-satisfies "1.4.0"]
```

Clojure CLI:

```clojure
  :deps {io.github.frenchy64/fully-satisfies 
         {:git/tag "1.4.0", :git/sha "c3ed93c"}}
```

Try it in a REPL:

```clojure
clj -Sdeps \
    '{:deps 
      {io.github.frenchy64/fully-satisfies 
       {:git/tag "1.4.0", :git/sha "c3ed93c"}}}'
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

Thanks [Wanderson Ferreira](https://github.com/wandersoncferreira) for the idea and name. My initial stance that this was impossible to implement quickly proved to be incorrect after Wanderson's asked the right questions and decompiled some bytecode.

Wanderson and [Mark Herman, II](https://github.com/turbodog99) also helped improve early iterations.

## Related work

- https://clojure.atlassian.net/browse/CLJ-2426
- https://clojure.atlassian.net/browse/CLJ-1814
- https://clojure.atlassian.net/browse/CLJ-2656
  - result of making this library and realizing `supers` call is suspicious

## License

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
