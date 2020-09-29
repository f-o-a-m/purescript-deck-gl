module DeckGL.Layer.Scatterplot where

import DeckGL (Layer)
import DeckGL.BaseProps (BaseProps)
import WebMercator.LngLat (LngLat)

foreign import defaultScatterplotProps :: ScatterplotLayerProps
foreign import makeScatterplotLayer :: ScatterplotLayerProps -> Layer

-- https://deck.gl/docs/api-reference/layers/scatterplot-layer
type ScatterplotData =
  { position :: LngLat
  , radius :: Number
  , color :: Array Int
  }

-- | - `stroked`: Only draw the stroked of the points
type ScatterplotLayerProps = BaseProps
  ( stroked :: Boolean
  )
  ScatterplotData
