{ name = "examples-icon"
, dependencies = (./spago.dhall).dependencies #
  [ "react-map-gl"
  , "web-html"
  , "affjax"
  , "argonaut"
  ]
, packages = (./spago.dhall).packages
, sources = (./spago.dhall).sources # ["examples/icon/src/**/*.purs"]
}
