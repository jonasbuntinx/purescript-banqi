{ name = "purescript-banqi"
, dependencies =
    [ "console"
    , "effect"
    , "node-readline"
    , "psci-support"
    , "random"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
