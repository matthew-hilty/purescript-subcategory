{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "subcategory"
, dependencies =
    [ "prelude"
    , "profunctor"
    , "proxy"
    , "record"
    , "typelevel-prelude"
    ]
, packages =
    ./packages.dhall
}
