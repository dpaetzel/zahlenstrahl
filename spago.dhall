{ name = "halogen-project"
, dependencies =
  [ "console"
  , "effect"
  , "halogen"
  , "prelude"
  , "psci-support"
  , "purescript-rationals"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
