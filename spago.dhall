{ name = "deck-gl"
, dependencies =
  [ "console"
  , "effect"
  , "foreign"
  , "foreign-object"
  , "prelude"
  , "psci-support"
  , "web-mercator"
  , "react-dom"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
