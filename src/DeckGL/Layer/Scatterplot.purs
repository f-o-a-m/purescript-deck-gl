module DeckGL.Layer.Scatterplot where

import DeckGL (Layer)
import DeckGL.BaseProps (BaseProps)
import MapGL (LngLat)

foreign import defaultScatterplotProps :: forall eff . ScatterplotLayerProps eff
foreign import makeScatterplotLayer :: forall eff . ScatterplotLayerProps eff -> Layer

type ScatterplotData =
    { position :: LngLat
    , radius :: Number
    , color :: Array Int
    }

-- | - `outline`: Only draw the outline of the points
-- | - `fp64`: Whether the layer should be rendered in high-precision 64-bit mode.
type ScatterplotLayerProps eff = BaseProps
    eff
    ( fp64 :: Boolean
    , outline :: Boolean
    )
    ScatterplotData
