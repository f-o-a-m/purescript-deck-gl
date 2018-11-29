module DeckGL.Layer.Scatterplot where

import DeckGL (Layer)
import DeckGL.BaseProps (BaseProps)
import WebMercator.LngLat (LngLat)

foreign import defaultScatterplotProps :: ScatterplotLayerProps
foreign import makeScatterplotLayer :: ScatterplotLayerProps -> Layer

type ScatterplotData =
  { position :: LngLat
  , radius :: Number
  , color :: Array Int
  }

-- | - `outline`: Only draw the outline of the points
-- | - `fp64`: Whether the layer should be rendered in high-precision 64-bit mode.
type ScatterplotLayerProps = BaseProps
  ( fp64 :: Boolean
  , outline :: Boolean
  )
  ScatterplotData
