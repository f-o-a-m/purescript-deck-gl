module DeckGL.Layer.ScreenGrid where

import DeckGL (Layer)
import DeckGL.BaseProps (BaseProps)
import WebMercator.LngLat (LngLat)

foreign import defaultScreenGridProps :: ScreenGridLayerProps
foreign import makeScreenGridLayer :: ScreenGridLayerProps -> Layer

type ScreenGridData =
  { position :: LngLat
  , weight :: Number
  }

-- | - `cellSizePixels`: Unit width/height of the bins.
-- | - `mincolor`: RGBA array, minimal color that can be rendered by a tile.
-- | - `maxcolor`: RGBA array, maximal color that can be rendered by a tile.
type ScreenGridLayerProps = BaseProps
  ( cellSizePixels :: Int
  , minColor :: Array Int
  , maxColor :: Array Int
  )
  ScreenGridData
