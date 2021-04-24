{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "behaviors"
  , "canvas"
  , "colors"
  , "console"
  , "effect"
  , "either"
  , "event"
  , "foldable-traversable"
  , "heterogeneous"
  , "identity"
  , "indexed-monad"
  , "integers"
  , "lists"
  , "math"
  , "maybe"
  , "painting"
  , "prelude"
  , "psci-support"
  , "record"
  , "sized-vectors"
  , "transformers"
  , "typelevel"
  , "typelevel-peano"
  , "wags"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
