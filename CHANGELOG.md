# 1.1.1-SNAPSHOT
- use Modifier class to decode method modifiers
- only call .getMethods when necessary

# 1.1.0 - 13th September 2021
- breaking changes: return false for values that don't explicitly extend an :extend-via-metadata protocol with zero methods

# 1.0.2
- walk inheritance chain to find implementation
- fix: nil should not inherit Object implementations
- fix: walk superclass chain even with :extend-via-metadata
- a value always implements an :extend-via-metadata protocol with zero methods

# 1.0.0 - 10th September 2021
- initial public release
