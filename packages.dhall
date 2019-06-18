{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let override =
  { packageName =
      upstream.packageName // { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName // { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen // { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom // { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { "package-name" =
       mkPackage
         [ "dependency1"
         , "dependency2"
         ]
         "https://example.com/path/to/git/repo.git"
         "tag ('v4.0.0') or branch ('master')"
  , "package-name" =
       mkPackage
         [ "dependency1"
         , "dependency2"
         ]
         "https://example.com/path/to/git/repo.git"
         "tag ('v4.0.0') or branch ('master')"
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
      mkPackage
        [ "arrays"
        , "exists"
        , "profunctor"
        , "strings"
        , "quickcheck"
        , "lcg"
        , "transformers"
        , "foldable-traversable"
        , "exceptions"
        , "node-fs"
        , "node-buffer"
        , "node-readline"
        , "datetime"
        , "now"
        ]
        "https://github.com/hdgarrood/purescript-benchotron.git"
        "v7.0.0"
  }
-------------------------------
-}

let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.3-20190315/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.3-20190315/src/packages.dhall sha256:08714bc666b16834f0f4cf86d408745ce005c43e3343821e4c3864ef28709177

let overrides =
  { argonaut-codecs =
      upstream.argonaut-codecs // { version = "v6.0.2" }
  , contravariant =
      upstream.contravariant // { version = "v4.0.1" }
  , lists =
      upstream.lists // { version = "v5.4.1" }
  , ordered-collections =
      upstream.ordered-collections // { version = "v1.6.1" }
  }

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
  , typelevel-eval =
      mkPackage
        [ "prelude"
        , "tuples"
        , "typelevel-prelude"
        ]
        "https://github.com/natefaubion/purescript-typelevel-eval.git"
        "v0.2.0"
  }

in  upstream // overrides // additions
