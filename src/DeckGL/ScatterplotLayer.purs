module DeckGL.ScatterplotLayer where

import DeckGL.BaseLayer (Layer, BaseLayerProps, LngLat)

foreign import defaultScatterplotProps :: forall eff . ScatterplotLayerProps eff
foreign import makeScatterplotLayer :: forall eff . ScatterplotLayerProps eff -> Layer

type ScatterplotData =
    { position :: LngLat
    , radius :: Number
    , color :: Array Int
    }

-- | - `outline`: Only draw the outline of the points
-- | - `fp64`: Whether the layer should be rendered in high-precision 64-bit mode.
type ScatterplotLayerProps eff = BaseLayerProps
    eff
    ( fp64 :: Boolean
    , outline :: Boolean
    )
    ScatterplotData
