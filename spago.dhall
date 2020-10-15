{ name = "purescript-banqi"
, dependencies =
    [ "console"
    , "effect"
    , "node-readline"
    , "psci-support"
    , "random"
    , "tailrec"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
