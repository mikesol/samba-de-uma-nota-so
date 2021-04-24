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
  , "event"
  , "heterogeneous"
  , "integers"
  , "lists"
  , "math"
  , "maybe"
  , "painting"
  , "prelude"
  , "psci-support"
  , "record"
  , "transformers"
  , "wags"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
