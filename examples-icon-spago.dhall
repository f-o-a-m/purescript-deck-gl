{ name = "examples-icon"
, dependencies = (./spago.dhall).dependencies #
  [ "react-map-gl"
  , "web-html"
  , "affjax"
  , "affjax-web"
  , "argonaut"
  , "aff"
  , "arrays"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "newtype"
  , "numbers"
  , "ordered-collections"
  , "partial"
  , "transformers"
  , "tuples"
  , "web-dom"
  ]
, packages = (./spago.dhall).packages
, sources = (./spago.dhall).sources # ["examples/icon/src/**/*.purs"]
}
