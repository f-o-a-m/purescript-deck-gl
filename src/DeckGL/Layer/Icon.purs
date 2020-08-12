module DeckGL.Layer.Icon where

import DeckGL (Layer)
import DeckGL.BaseProps (BaseProps)
import WebMercator.LngLat (LngLat)
import Foreign.Object (Object)

foreign import defaultIconProps :: forall d . IconLayerProps d
foreign import makeIconLayer :: forall d . IconLayerProps d -> Layer

-- | An `Icon` represents a datapoint in a layer. The image for the icon is called an
-- | "atlas", and the location and dimension of the icon in the atlas are given by
-- | the properties. This is useful if you have multiple different icons to display, because
-- | the layer must use the same image for all the different icons.
-- | The `mask` is a transparancy mask, if true the user defined color is applied,
-- | otherwise the color from the image is applied.
type Icon =
  { x :: Int
  -- ^  x position of icon on the atlas image
  , y :: Int
  -- ^  y position of icon on the atlas image
  , width :: Int
  -- ^ width of icon on the atlas image
  , height :: Int
  -- ^ height of icon on the atlas image
  , mask :: Boolean
  -- ^ whether icon is treated as a transparency
  }

type IconMapping = Object Icon

type IconData d = {| d}

-- | - `iconAtlas`: The url of the image for the icon atlas.
-- | - `iconMapping`: A mapping of icon names to `Icon`s.
-- | - `sizeScale`: Icon size multiplier.
type IconLayerProps d = BaseProps
  ( iconAtlas :: String
  , iconMapping :: IconMapping
  , sizeScale :: Number
  , getPosition :: IconData d -> LngLat
  , getSize :: IconData d -> Number
  , getIcon :: IconData d -> String
  , getColor :: IconData d -> Array Number
  , getAngle :: IconData d -> Number
  )
  (IconData d)
