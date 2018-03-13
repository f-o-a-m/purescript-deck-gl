module DeckGL where

import React (ReactClass)

foreign import data GLInitializer :: Type

foreign import initializeGL :: GLInitializer

type DeckGLProps =
  { width :: Int
  , height :: Int
  , latitude :: Number
  , longitude :: Number
  , zoom :: Number
  , bearing :: Number
  , pitch :: Number
  , initializer :: GLInitializer
  }

foreign import deckGL :: ReactClass DeckGLProps
