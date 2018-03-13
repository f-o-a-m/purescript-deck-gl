module DeckGL.Projection where

import MapGL (Viewport, LngLat)

foreign import data MercatorProjector :: Type

foreign import makeMercatorProjector :: Viewport -> MercatorProjector

type ScreenCoordinates =
  { x :: Int
  , y :: Int
  }

foreign import unproject :: MercatorProjector -> ScreenCoordinates -> LngLat
foreign import project :: MercatorProjector -> LngLat -> ScreenCoordinates
