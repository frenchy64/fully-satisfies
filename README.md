# fully-satisfies

Provides a variant of `clojure.core/satisfies?` that also checks if all methods are implemented.

## Dependency

Leiningen:

```clojure
[io.github.frenchy64/fully-satisfies "TBD"]
```

Clojure CLI:

```clojure
  :deps {io.github.frenchy64/fully-satisfies 
         {:git/sha "f1709cfe416dc89cd62a1bcf6f2b36ebd99f3cfd"}}
```

## Usage

```clojure
(require '[io.github.frenchy64.fully-satisfies :refer [fully-satisfies?]])

(defprotocol A
  (a [this])
  (b [this]))

(fully-satisfies? A (reify))
;=> false
(fully-satisfies? A (reify A)))
;=> false
(fully-satisfies? A (reify A (a [this]))))
;=> false
(fully-satisfies? A (reify A (a [this]) (b [this]))))
;=> true
```

## Releasing

1. Commit with message `Release {:major,:minor,:patch}`
2. Pull
3. Update `README.md`

## Thanks

Thanks [Wanderson Ferreira](https://github.com/wandersoncferreira) for the idea and name. My initial stance that this was impossible to implement quickly proved to be incorrect after Wanderson's asked the right questions and decompiled some bytecode.

Wanderson and [Mark Herman, II](https://github.com/turbodog99) also helped improve early iterations.

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
