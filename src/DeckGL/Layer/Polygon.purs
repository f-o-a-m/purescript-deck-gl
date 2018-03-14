module DeckGL.Layer.Polygon where

import DeckGL (Layer)
import DeckGL.BaseProps (BaseProps)
import MapGL (LngLat)

foreign import defaultPolygonProps :: forall eff. PolygonLayerProps eff

foreign import makePolygonLayer :: forall eff . PolygonLayerProps eff -> Layer

type PolygonData =
  { polygon :: Array LngLat
  , fillColor :: Array Int
  , strokeColor :: Array Int
  , strokeWidth :: Int
  , color :: Array Int
  }

type PolygonLayerProps eff = BaseProps
    eff
    (stroked :: Boolean
    ,lineWidthMinPixels :: Int
    ,fp64 :: Boolean
    )
    PolygonData
