{ name = "purescript-cofree-zipper"
, dependencies =
  [ "console"
  , "effect"
  , "free"
  , "leibniz"
  , "lists"
  , "matryoshka"
  , "maybe"
  , "prelude"
  , "profunctor-lenses"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
