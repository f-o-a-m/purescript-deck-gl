module DeckGL.Layer.Polygon where

import DeckGL (Layer)
import DeckGL.BaseProps (BaseProps)
import WebMercator.LngLat (LngLat)

foreign import defaultPolygonProps :: PolygonLayerProps

foreign import makePolygonLayer :: PolygonLayerProps -> Layer

type PolygonData =
  { polygon :: Array LngLat
  , fillColor :: Array Int
  , strokeColor :: Array Int
  , strokeWidth :: Int
  , color :: Array Int
  }

type PolygonLayerProps = BaseProps
  ( stroked :: Boolean
  , lineWidthMinPixels :: Int
  , fp64 :: Boolean
  )
  PolygonData
