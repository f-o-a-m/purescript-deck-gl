module DeckGL.Layer.Icon where

import DeckGL (Layer)
import DeckGL.BaseProps (BaseProps)
import WebMercator.LngLat (LngLat)
import Data.StrMap

foreign import defaultIconProps :: forall d eff . IconLayerProps d eff
foreign import makeIconLayer :: forall d eff . IconLayerProps d eff -> Layer

-- | An `Icon` represents a datapoint in a layer. The image for the icon is called an
-- | "atlas", and the location and dimension of the icon in the atlas are given by
-- | the properties. This is useful if you have multiple different icons to display, because
-- | the layer must use the same image for all the different icons.
-- | The `mask` is a transparancy mask, if true the user defined color is applied,
-- | otherwise the color from the image is applied.
type Icon =
  { x :: Int
  , y :: Int
  , width :: Int
  , height :: Int
  , mask :: Boolean
  }

type IconMapping = StrMap Icon

type IconData d = {| d}

-- | - `iconAtlas`: The url of the image for the icon atlas.
-- | - `iconMapping`: A mapping of icon names to `Icon`s.
-- | - `sizeScale`: Icon size multiplier.
type IconLayerProps d eff = BaseProps
  eff
  ( iconAtlas :: String
  , iconMapping :: IconMapping
  , sizeScale :: Number
  , getPosition :: IconData d -> LngLat
  , getSize :: IconData d -> Number
  , getIcon :: IconData d -> String
  )
  (IconData d)
