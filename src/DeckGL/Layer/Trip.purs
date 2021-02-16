module DeckGL.Layer.Trip where

import DeckGL (Layer)
import DeckGL.BaseProps (BaseProps)
import WebMercator.LngLat (LngLat)

foreign import defaultTripsProps :: forall d. TripsLayerProps d

foreign import makeTripsLayer :: forall d. TripsLayerProps d -> Layer

type TripsData d
  = { | d }

type TripsLayerProps d
  = BaseProps
      ( currentTime :: Number
      , trailLength :: Number
      , getPath :: TripsData d -> Array LngLat
      , getTimestamps :: TripsData d -> Array Number
      , getTrip :: TripsData d -> String
      , getColor :: TripsData d -> Array Number
      )
      (TripsData d)
