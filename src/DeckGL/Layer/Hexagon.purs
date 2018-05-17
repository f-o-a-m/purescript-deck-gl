module DeckGL.Layer.Hexagon where

import DeckGL (Layer)
import DeckGL.BaseProps (BaseProps, LightSettings)
import WebMercator.LngLat (LngLat)

foreign import defaultHexagonProps :: forall eff . HexagonLayerProps eff

foreign import makeHexagonLayer :: forall eff . HexagonLayerProps eff -> Layer

type HexData =
  { position :: Array LngLat
  }

-- | - `coverage`: The final radius of hexagon is calculated by coverage * radius. Note:
-- | coverage does not affect how points are binned. The radius of the bin is determined
-- | only by the radius property.
-- | - `colorRange`: A tuple of numbers indicating the min and max range of elevation.
-- | - `elevationScale`: Hexagon elevation multiplier. The actual elevation is calculated
-- | by elevationScale * getElevation(d). elevationScale is a handy property to scale all
-- | hexagons without updating the data.
-- | `extruded`: Whether to enable cell elevation. Cell elevation scale by count of points
-- | in each cell. If set to false, all cells will be flat.
-- | `radius`: Radius of hexagons measured in meters.

type HexagonLayerProps eff = BaseProps
    eff
    ( coverage :: Int
    , colorRange :: Array (Array Int)
    , elevationRange :: Array Int
    , elevationScale :: Int
    , extruded :: Boolean
    , lightSettings :: LightSettings
    , radius :: Int
    , upperPercentile :: Int
    )
    HexData
