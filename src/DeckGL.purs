module DeckGL where

import Prelude

import Data.Nullable (Nullable)
import Effect.Uncurried (EffectFn2)
import Foreign (Foreign)
import React (ReactClass)
import Unsafe.Coerce (unsafeCoerce)
import WebMercator.LngLat (LngLat)
import WebMercator.Pixel (Pixel)
import WebMercator.Viewport (ViewportR)

foreign import data Layer :: Type
foreign import data GLInitializer :: Type
foreign import data MouseEvent :: Type
foreign import data LayerInfo :: Type

layer :: LayerInfo -> Layer
layer = unsafeCoerce >>> _.layer

layerId :: Layer -> String
layerId = unsafeCoerce >>> _.id

lngLat :: LayerInfo -> LngLat
lngLat = unsafeCoerce >>> _.lngLat

object :: LayerInfo -> Nullable Foreign
object = unsafeCoerce >>> _.object

pixel :: LayerInfo -> Pixel
pixel = unsafeCoerce >>> _.pixel


foreign import defaultDeckGLProps :: DeckGLProps

type MapPropsR r =
  ( layers :: Array Layer
  , onClick :: EffectFn2 (Nullable LayerInfo) MouseEvent Unit
  , onHover :: EffectFn2 (Nullable LayerInfo) MouseEvent Unit
  | r
  )

type DeckGLProps = Record (MapPropsR (ViewportR ()))

foreign import deckGL :: ReactClass (DeckGLProps)

