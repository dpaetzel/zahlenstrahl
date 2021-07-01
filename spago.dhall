{ name = "halogen-project"
, dependencies =
  [ "arrays"
  , "canvas"
  , "canvas-action"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-bootstrap4"
  , "integers"
  , "math"
  , "maybe"
  , "numbers"
  , "partial"
  , "polymorphic-vectors"
  , "prelude"
  , "psci-support"
  , "strings"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
