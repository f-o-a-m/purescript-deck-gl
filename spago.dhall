{ name = "deck-gl"
, dependencies =
  [ "console"
  , "effect"
  , "foreign"
  , "foreign-object"
  , "nullable"
  , "prelude"
  , "psci-support"
  , "web-mercator"
  , "react"
  , "react-dom"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
