let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2-20190725/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2-20190725/src/packages.dhall sha256:60cc03d2c3a99a0e5eeebb16a22aac219fa76fe6a1686e8c2bd7a11872527ea3

let overrides = {=}

let additions =
  { lens =
      mkPackage
        [ "const"
        , "distributive"
        , "profunctor"
        ]
        "https://github.com/purescript-contrib/purescript-lens.git"
        "v4.0.0"
  , precise-datetime =
      mkPackage
        [ "arrays"
        , "console"
        , "datetime"
        , "decimals"
        , "either"
        , "enums"
        , "foldable-traversable"
        , "formatters"
        , "integers"
        , "js-date"
        , "lists"
        , "maybe"
        , "newtype"
        , "numbers"
        , "prelude"
        , "strings"
        , "tuples"
        , "unicode"
        ]
        "https://github.com/awakesecurity/purescript-precise-datetime.git"
        "v5.1.1"
  , slug =
      mkPackage
        [ "argonaut-codecs"
        , "generics-rep"
        , "maybe"
        , "prelude"
        , "strings"
        , "unicode"
        ]
        "https://github.com/thomashoneyman/purescript-slug.git"
        "v1.0.0"
  }

in  upstream // overrides // additions
