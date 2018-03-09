module DeckGL where

import React (ReactClass)

foreign import data GLInitializer :: Type

foreign import initializeGL :: GLInitializer

foreign import deckGL :: forall props. ReactClass props
