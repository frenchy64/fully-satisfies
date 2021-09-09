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
         {:git/sha "b09d53786cea805627f246bc4e4b08c7fd3c9e40"}}
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
(fully-satisfies? A (reify A (aA [this]))))
;=> false
(fully-satisfies? A (reify A (aA [this]) (bA [this]))))
;=> true
```

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
