module DeckGL.Layer.Polygon where

import DeckGL (Layer)
import DeckGL.BaseProps (BaseProps)
import WebMercator.LngLat (LngLat)

foreign import defaultPolygonProps :: forall d. PolygonLayerProps d
foreign import makePolygonLayer :: forall d. PolygonLayerProps d -> Layer

type PolygonData d = {|d}

type PolygonLayerProps d = BaseProps
  ( getPolygon :: PolygonData d -> Array LngLat
  , getFillColor :: PolygonData d -> Array Int
  , getElevation :: PolygonData d -> Int
  , getLineWidth :: PolygonData d -> Int
  , getLineColor :: PolygonData d -> Array Int
  , filled :: Boolean
  , extruded :: Boolean
  , lineWidthMinPixels :: Int
  , material :: Boolean
  )
  (PolygonData d)
