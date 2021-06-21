{ name = "halogen-project"
, dependencies =
  [ "arrays"
  , "canvas"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "integers"
  , "math"
  , "maybe"
  , "numbers"
  , "partial"
  , "prelude"
  , "psci-support"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
