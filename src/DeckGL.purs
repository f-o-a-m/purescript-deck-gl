module DeckGL where

import Prelude

import Effect.Uncurried (EffectFn1)
import React (ReactClass)
import WebMercator.LngLat (LngLat)
import WebMercator.Viewport (ViewportR)

foreign import data Layer :: Type
foreign import data GLInitializer :: Type
foreign import data MouseEvent :: Type
foreign import data LayerInfo :: Type

foreign import defaultDeckGLProps :: DeckGLProps

type MapPropsR r =
  ( layers :: Array Layer
  | r
  )

type DeckGLProps = Record (MapPropsR (ViewportR ()))

foreign import deckGL :: ReactClass (DeckGLProps)

