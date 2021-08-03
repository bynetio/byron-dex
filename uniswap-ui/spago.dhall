{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "uniswap"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "codec"
  , "codec-argonaut"
  , "console"
  , "const"
  , "datetime"
  , "dom-indexed"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "formatters"
  , "halogen"
  , "halogen-formless"
  , "halogen-store"
  , "http-methods"
  , "integers"
  , "maybe"
  , "newtype"
  , "now"
  , "prelude"
  , "profunctor"
  , "psci-support"
  , "remotedata"
  , "routing"
  , "routing-duplex"
  , "safe-coerce"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "variant"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
