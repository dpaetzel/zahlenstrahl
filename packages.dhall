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
