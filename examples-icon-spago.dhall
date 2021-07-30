{ name = "examples-icon"
, dependencies =
      (./spago.dhall).dependencies
    # [ "react-map-gl"
      , "web-html"
      , "affjax"
      , "argonaut"
      , "aff"
      , "arrays"
      , "either"
      , "exceptions"
      , "foldable-traversable"
      , "integers"
      , "math"
      , "maybe"
      , "newtype"
      , "ordered-collections"
      , "partial"
      , "react-dom"
      , "transformers"
      , "tuples"
      , "web-dom"
      ]
, packages = (./spago.dhall).packages
, sources = (./spago.dhall).sources # [ "examples/icon/src/**/*.purs" ]
}
