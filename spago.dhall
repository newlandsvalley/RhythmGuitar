{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "rhythm-guitar"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "arrays"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "foreign"
  , "http-methods"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "soundfonts"
  , "string-parsers"
  , "strings"
  , "tuples"
  , "yoga-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/newlandsvalley/rhythm-guitar"
}
