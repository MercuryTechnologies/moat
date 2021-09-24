# moat

## Generate Kotlin and Swift types from Haskell types

TODO: fill this out with examples

## Useful tidbits

To see the TH AST for a given type

```
:set -XTemplateHaskell
$(stringE . show =<< reifyDatatype ''A)
```

`''A` takes a type constructor `A` and converts it to a `Name`.  We then bind
from the `Q` monad to generate a string expression via `stringE` and `show` it
