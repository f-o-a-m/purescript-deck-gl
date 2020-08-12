module DeckGL.Layer.ScreenGrid where

import DeckGL (Layer)
import DeckGL.BaseProps (BaseProps)
import WebMercator.LngLat (LngLat)

foreign import defaultScreenGridProps :: forall d. ScreenGridLayerProps d
foreign import makeScreenGridLayer :: forall d. ScreenGridLayerProps d -> Layer

type ScreenGridData d = {|d}

-- | - `cellSizePixels`: Unit width/height of the bins (in [1, 100]).
type ScreenGridLayerProps d = BaseProps
  ( cellSizePixels :: Int
  , gpuAggregation :: Boolean
  , getPosition :: ScreenGridData d -> LngLat
  , getWeight :: ScreenGridData d -> Number
  )
  (ScreenGridData d)
