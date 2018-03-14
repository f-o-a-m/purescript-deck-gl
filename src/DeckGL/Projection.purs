module DeckGL.Projection where

import Prelude
import MapGL (Viewport(..), LngLat, lng, lat)

foreign import data MercatorProjector :: Type

foreign import makeMercatorProjector :: Viewport -> MercatorProjector

type ScreenCoordinates =
  { x :: Int
  , y :: Int
  }

foreign import unproject :: MercatorProjector -> ScreenCoordinates -> LngLat
foreign import project :: MercatorProjector -> LngLat -> ScreenCoordinates

getBoundingBox :: Viewport
               -> { sw :: {lat :: Number, lng :: Number}
                  , ne :: {lat :: Number, lng :: Number}
                  }
getBoundingBox v@(Viewport vp) =
    let unprojector = unproject (makeMercatorProjector v)
    in { sw: makeCoords $ unprojector {x: 0, y: vp.height}
       , ne: makeCoords $ unprojector {x: vp.width, y: 0}
       }
  where
    makeCoords lngLat = {lng: lng lngLat, lat: lat lngLat}
