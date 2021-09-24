# moat

## Given Haskell Types Generate Equivalents in Your Frontend Language

```haskell
-- This extension generates Haskell code 
{-# LANGUAGE TemplateHaskell #-}
-- These extensions for the generated code to compile
{-# LANGUAGE ScopedTypeVariables, DataKinds #-}

import Moat (mobileGen)

data User =
  User 
    { firstName :: String
    , lastName :: String
    , age :: Int
    }

$(mobileGen ''User)
```

The function `mobileGen` will build two instances for you: `ToMoatType` and `ToMoatData`.
Using these instance we can generate code for your favorite language!

```haskell
{-# LANGUAGE TypeApplications #-}

import Moat (prettySwiftData)
import Data.Proxy (Proxy (..))

generatedSwiftCode :: String
generatedSwiftCode = prettySwiftData . toMoatData $ Proxy @User
```

Would generate the following Swift code

```swift
struct User {
    let firstName: String
    let lastName: String
    let age: Int
}
```

## Motivation

At Mercury, we want a single source of truth for all the types in our
applications. Haskell has a very strong type system and therefore we choose
this as our source of truth. Most programming languages have libraries or
built-ins to deserialize JSON, e.g. `Parcelize` in Android (Kotlin) and
`Codable` in Swift, which we use as our interchange format. This allows us to
automatically convert our Haskell types to the frontend equivalents and be sure
everyone is on the same page.

![A moat use case diagram](./diagram/moat-use-case.png "Moat use case")

Here is a more practical Swift example that uses `Codable` to decode JSON
blobs.

```haskell
import Moat (mobileGenWith, defaultOptions, Options (..))

$(mobileGenWith defaultOptions {dataProtocols = [Codable]} ''User)
```

which will generate the following Swift code,

```swift
struct User: Codable {
    let firstName: String
    let lastName: String
    let age: Int
}
```

For Android, one can use the `kotlin-parcelize` package (using `prettyKotlinData`), i.e.

```haskell
import Moat (mobileGenWith, defaultOptions, Options (..))

$(mobileGenWith defaultOptions {dataInterfaces = [Parcelable], dataAnnotations = [Parcelize]} ''User)
```

```kotlin
@Parcelize
data class User(
    val firstName: String,
    val lastName: String,
    val age: Int,
) : Parcelable
```
## FAQ

### Why Template Haskell

Template Haskell is already prevalent in our Haskell codebase. It would
increase our compile times drastically to additionally use `Generic` to derive AST
for each of our Haskell types.

## Useful tidbits

To see the TH AST for a given type

```
:set -XTemplateHaskell
$(stringE . show =<< reifyDatatype ''A)
```

`''A` takes a type constructor `A` and converts it to a `Name`.  We then bind
from the `Q` monad to generate a string expression via `stringE` and `show` it
