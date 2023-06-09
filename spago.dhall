{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "canvas"
  , "canvas-action"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "halogen"
  , "halogen-bootstrap4"
  , "halogen-subscriptions"
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
  , "tailrec"
  , "web-cssom"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
