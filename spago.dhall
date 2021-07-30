{ name = "deck-gl"
, dependencies =
  [ "console"
  , "effect"
  , "foreign"
  , "foreign-object"
  , "nullable"
  , "prelude"
  , "psci-support"
  , "react"
  , "unsafe-coerce"
  , "web-mercator"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
