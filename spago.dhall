{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "canvas"
  , "canvas-action"
  , "console"
  , "effect"
  , "either"
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
  , "refs"
  , "strings"
  , "web-cssom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
