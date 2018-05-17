module DeckGL.Layer.ScreenGrid where

import DeckGL (Layer)
import DeckGL.BaseProps (BaseProps)
import WebMercator.LngLat (LngLat)

foreign import defaultScreenGridProps :: forall eff . ScreenGridLayerProps eff
foreign import makeScreenGridLayer :: forall eff . ScreenGridLayerProps eff -> Layer

type ScreenGridData =
  { position :: LngLat
  , weight :: Number
  }

-- | - `cellSizePixels`: Unit width/height of the bins.
-- | - `mincolor`: RGBA array, minimal color that can be rendered by a tile.
-- | - `maxcolor`: RGBA array, maximal color that can be rendered by a tile.
type ScreenGridLayerProps eff = BaseProps
    eff
    ( cellSizePixels :: Int
    , minColor :: Array Int
    , maxColor :: Array Int
    )
    ScreenGridData
