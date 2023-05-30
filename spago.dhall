{ name = "purescript-cofree-zipper"
, dependencies =
  [ "console"
  , "effect"
  , "foldable-traversable"
  , "free"
  , "leibniz"
  , "lists"
  , "matryoshka"
  , "maybe"
  , "prelude"
  , "profunctor-lenses"
  , "tuples"
  , "type-equality"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
