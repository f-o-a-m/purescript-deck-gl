module DeckGL.PolygonLayer where

import DeckGL.BaseLayer (Layer, BaseLayerProps, LngLat)

foreign import defaultPolygonProps :: forall eff. PolygonLayerProps eff

foreign import makePolygonLayer :: forall eff . PolygonLayerProps eff -> Layer

type PolygonData =
  { polygon :: Array LngLat
  , fillColor :: Array Int
  , strokeColor :: Array Int
  , strokeWidth :: Int
  , color :: Array Int
  }

type PolygonLayerProps eff = BaseLayerProps
    eff
    (stroked :: Boolean
    ,lineWidthMinPixels :: Int
    ,fp64 :: Boolean
    )
    PolygonData
