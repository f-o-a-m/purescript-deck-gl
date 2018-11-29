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
-- | - `fp64`: Whether the layer should be rendered in high-precision 64-bit mode
type ArcLayerProps = BaseProps
  ( strokeWidth :: Int
  , fp64 :: Boolean
  )
  ArcData
