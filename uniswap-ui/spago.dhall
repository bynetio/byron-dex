{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "uniswap"
, dependencies =
  [ "aff"
  , "argonaut-core"
  , "codec"
  , "codec-argonaut"
  , "console"
  , "effect"
  , "either"
  , "halogen"
  , "halogen-formless"
  , "halogen-store"
  , "maybe"
  , "newtype"
  , "now"
  , "prelude"
  , "profunctor"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  , "safe-coerce"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
