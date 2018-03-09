module DeckGL.Layer where

import Control.Monad.Eff.Uncurried (EffFn1)

foreign import data Layer :: Type
foreign import data LayerClass :: Type

-- | The deck.gl representation of a longitude latitude pair is
-- | an array of two numbers.
foreign import data LngLat :: Type

foreign import getLat :: LngLat -> Number
foreign import getLng :: LngLat -> Number
foreign import mkLngLat :: Number -> Number -> LngLat
foreign import mkLngLatElev :: Number -> Number -> Number -> LngLat

type PickingInfo a =
  { layer :: Layer
  , index :: Int
  , object :: a -- TODO: for some layers this is not actually an `a`
  , x :: Int
  , y :: Int
  , lngLat :: LngLat
  }

type BaseLayerProps eff rest a =
  { id :: String
  , data :: Array a
  , visible :: Boolean
  , opacity :: Number
  , pickable :: Boolean
  , onHover :: EffFn1 eff (PickingInfo a) Boolean
  , onClick :: EffFn1 eff (PickingInfo a) Boolean
  , autoHighlight :: Boolean
  , highlightedObjectIndex :: Int
  | rest
  }
