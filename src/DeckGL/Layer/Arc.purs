module DeckGL.Layer.Arc where

import DeckGL (Layer)
import DeckGL.BaseProps (BaseProps)
import WebMercator.LngLat (LngLat)

foreign import defaultArcProps :: ArcLayerProps
foreign import makeArcLayer :: ArcLayerProps -> Layer

type ArcData =
  { sourcePosition :: LngLat
  , targetPosition :: LngLat
  , color :: Array Int
  }

-- | - `strokeWidth`: used to draw each arc. Unit is pixels.
type ArcLayerProps = BaseProps
  ( strokeWidth :: Int
  )
  ArcData
