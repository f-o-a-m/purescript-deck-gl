module DeckGL.IconLayer where

import DeckGL.Layer (Layer, BaseLayerProps)
import Data.StrMap

foreign import mkLayer :: forall d eff . IconLayerProps d eff -> Layer

type Icon =
  { x :: Int
  , y :: Int
  , width :: Int
  , height :: Int
  , mask :: Boolean
  }

type IconMapping = StrMap Icon

type IconData d =
  { position :: Array Number
  , icon :: String
  , size :: Int
  | d
  }

type IconLayerProps d eff = BaseLayerProps
  eff
  ( iconAtlas :: String
  , iconMapping :: IconMapping
  , getColor :: IconData d -> Array Int
  , sizeScale :: Number
  )
  (IconData d)
