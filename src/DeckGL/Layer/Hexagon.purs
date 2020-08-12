module DeckGL.Layer.Hexagon where

import DeckGL (Layer)
import DeckGL.BaseProps (BaseProps)
import WebMercator.LngLat (LngLat)

foreign import defaultHexagonProps :: forall d. HexagonLayerProps d
 
foreign import makeHexagonLayer :: forall d. HexagonLayerProps d -> Layer

type HexData d = {|d}

-- | - `coverage`: The final radius of hexagon is calculated by coverage * radius. Note:
-- | coverage does not affect how points are binned. The radius of the bin is determined
-- | only by the radius property.
-- | - `elevationRange`: A tuple of numbers indicating the min and max range of elevation.
-- | - `elevationScale`: Hexagon elevation multiplier. The actual elevation is calculated
-- | by elevationScale * getElevation(d). elevationScale is a handy property to scale all
-- | hexagons without updating the data.
-- | `extruded`: Whether to enable cell elevation. Cell elevation scale by count of points
-- | in each cell. If set to false, all cells will be flat.
-- | `radius`: Radius of hexagons measured in meters.

type HexagonLayerProps d = BaseProps
  ( getPosition :: HexData d -> LngLat
  , lowerPercentile :: Number
  , upperPercentile :: Number
  , elevationLowerPercentile :: Number
  , elevationUpperPercentile :: Number
  , elevationRange :: Array Int
  , elevationScale :: Int
  , coverage :: Int
  , colorRange :: Array (Array Int)
  , extruded :: Boolean
  , radius :: Int
  , material ::Boolean
  )
  (HexData d)
