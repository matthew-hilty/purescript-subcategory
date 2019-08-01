{-
Welcome to a Spago project!
You can edit this file as you like.
-}
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
