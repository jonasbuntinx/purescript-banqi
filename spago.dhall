{ name = "purescript-banqi"
, dependencies =
    [ "console"
    , "effect"
    , "node-readline"
    , "ordered-collections"
    , "psci-support"
    , "random"
    , "tailrec"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
