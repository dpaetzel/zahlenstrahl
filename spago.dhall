{ name = "halogen-project"
, dependencies =
  [ "canvas"
  , "console"
  , "effect"
  , "halogen"
  , "prelude"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
