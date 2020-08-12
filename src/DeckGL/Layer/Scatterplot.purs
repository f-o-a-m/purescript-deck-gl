module DeckGL.Layer.Scatterplot where

import DeckGL (Layer)
import DeckGL.BaseProps (BaseProps)
import WebMercator.LngLat (LngLat)

foreign import defaultScatterplotProps :: forall d. ScatterplotLayerProps d
foreign import makeScatterplotLayer :: forall d. ScatterplotLayerProps d -> Layer

type ScatterplotData d = {|d}

-- | - `outline`: Only draw the outline of the points
-- | - `fp64`: Whether the layer should be rendered in high-precision 64-bit mode.
type ScatterplotLayerProps d = BaseProps
  ( getPosition :: ScatterplotData d -> LngLat
  , getRadius :: Number
  , getFillColor :: Array Int
  , getLineColor :: Array Int
  , getLineWidth :: Array Int
  , stroked :: Boolean
  , filled :: Boolean
  , radiusMinPixels :: Int
  , lineWidthMinPixels :: Int
  )
  (ScatterplotData d)
