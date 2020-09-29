{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "deck-gl"
, dependencies =
  [ "console"
  , "effect"
  , "foreign"
  , "foreign-object"
  , "prelude"
  , "psci-support"
  , "web-mercator"

  -- dev
  , "react-dom"
  , "react-map-gl"
  , "web-html"
  , "affjax"
  , "argonaut"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
