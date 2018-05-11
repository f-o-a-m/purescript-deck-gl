module DeckGL where

import React (ReactClass)
import WebMercator.Viewport (ViewportR)

foreign import data Layer :: Type
foreign import data GLInitializer :: Type

foreign import initializeGL :: GLInitializer

type MapPropsR r =
  ( layers :: Array Layer
  , initializer :: GLInitializer
  | r
  )

type DeckGLProps = Record (MapPropsR (ViewportR ()))

foreign import deckGL :: ReactClass DeckGLProps
