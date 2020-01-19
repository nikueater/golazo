{ name = "golazo"
, dependencies =
    [ "console"
    , "effect"
    , "lists"
    , "node-fs-aff"
    , "ordered-collections"
    , "parsing"
    , "psci-support"
    ]
, packages = ./packages.dhall
, sources = [  "src/**/*.purs", "test/**/*.purs" ]
}
