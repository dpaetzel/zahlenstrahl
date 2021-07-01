let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210309/packages.dhall sha256:585332a8a11c6420d7287943f81bc2121746cdd352f2cf3f5ecf65053f2afcd3

in  upstream
  with halogen-bootstrap4 =
    -- I just copied these from the spago.dhall from that project
    { dependencies = [ "console", "effect", "halogen", "psci-support" ]
    , repo =
        "https://github.com/mschristiansen/purescript-halogen-bootstrap4.git"
    , version =
        "v0.2.0"
    }
  with canvas-action =
    -- I just copied these from the spago.dhall from that project
    { dependencies =
      [ "aff"
      , "arrays"
      , "canvas"
      , "colors"
      , "effect"
      , "either"
      , "exceptions"
      , "foldable-traversable"
      , "math"
      , "maybe"
      , "numbers"
      , "polymorphic-vectors"
      , "prelude"
      , "refs"
      , "run"
      , "transformers"
      , "tuples"
      , "type-equality"
      , "typelevel-prelude"
      , "unsafe-coerce"
      , "web-dom"
      , "web-events"
      , "web-html"
      ]
    , repo =
        "https://github.com/artemisSystem/purescript-canvas-action"
    , version =
        "v7.0.0"
    }
  with polymorphic-vectors =
    { dependencies =
      [ "distributive"
      , "foldable-traversable"
      , "math"
      , "prelude"
      , "record"
      , "typelevel-prelude"
      ]
    , repo =
        "https://github.com/artemisSystem/purescript-polymorphic-vectors"
    , version =
        "v3.0.0"
    }
