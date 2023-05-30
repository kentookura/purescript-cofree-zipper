let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230530/packages.dhall
        sha256:84e7222623e0ea6482171929ab6ec359a7fa37ae0a7066f4407667f233686eb6

in  upstream
  with leibniz = 
    { repo = "https://github.com/kentookura/purescript-leibniz.git"
    , version = "3ef1ae17e572073ebce2d847873740bb63e75be3"
    , dependencies = ["console", "effect", "exists", "prelude", "unsafe-coerce"]
    }