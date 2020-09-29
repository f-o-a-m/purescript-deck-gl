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
type ScatterplotLayerProps = BaseProps
  ( outline :: Boolean
  )
  ScatterplotData
