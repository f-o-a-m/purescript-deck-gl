module DeckGL.ArcLayer where

import DeckGL.BaseLayer (BaseLayerProps, Layer, LngLat)

foreign import defaultArcProps :: forall eff . ArcLayerProps eff
foreign import makeArcLayer :: forall eff . ArcLayerProps eff -> Layer

type ArcData =
    { sourcePosition :: LngLat
    , targetPosition :: LngLat
    , color :: Array Int
    }

-- | - `strokeWidth`: used to draw each arc. Unit is pixels.
-- | - `fp64`: Whether the layer should be rendered in high-precision 64-bit mode
type ArcLayerProps eff = BaseLayerProps
    eff
    ( strokeWidth :: Int
    , fp64 :: Boolean
    )
    ArcData
