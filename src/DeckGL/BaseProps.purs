module DeckGL.BaseProps where

import DeckGL (Layer)
import WebMercator.LngLat (LngLat)
import Effect.Uncurried (EffectFn1)


-- | The picking engine returns objects of type `PickingInfo a` describing what
-- | layer and object were picked.
-- | - `layer`: The layer that was picked.
-- | - `index`: The index of the object in the layer that was picked.
-- | - `object`: The object that was picked, typically in the layer's `props.data` array, but can vary.
-- | - `x`: Mouse position `x` relative to the viewport.
-- | - `y`: Mouse position `y` relative to the viewport.
-- | - `lnglat`: Mouse position in geospatial coordinates. Only applies if layer.props.coordinateSystem
-- |   is a geospatial mode such as `COORDINATE_SYSTEM.LNGLAT`
type PickingInfo a =
  { layer :: Layer
  , index :: Int
  , object :: a
  , x :: Int
  , y :: Int
  , lngLat :: LngLat
  }

-- | Some layers require light settings to customize the rendering.
type LightSettings =
  { lightsPosition :: Array Number
  , ambientRatio :: Number
  , diffuseRatio :: Number
  , specularRatio :: Number
  , lightsStrength :: Array Number
  , numberOfLights :: Int
  }

-- | `BaseProps rest a` are basic properties that all layers share. There are
-- | many properties, but they all have sensible defaults in the library should you choose
-- | not to set them. See https://github.com/uber/deck.gl/blob/master/docs/api-reference/layer.md.
-- | - `id`: A unique name for the layer.
-- | - `data`: The array of data that the layer contains.
-- | - `visibility`: Controls the visibility of the layer, should use instead of conditional rendering.
-- | - `opacity`: The opacity of the layer.
-- | - `pickable`: Whether the layer responds to mouse picking events.
-- | - `fp64`: Whether the layer should be rendered in high-precision 64-bit mode
-- | - `onHover`: Callback that gets called when hovering on an object.
-- | - `onClick`: Callback for clicking on an object in a layer.
-- | - `autoHighlight`: When true, the current object pointed to by the mouse on hover is highlighted with the color
-- | - `highlightColor`: RGBA array for highlighted objects.
-- | Note: If a callback returns a truthy value, the hover event is marked as handled and will not bubble up to
-- | the onLayerClick callback of the DeckGL canvas.
type BaseProps rest a =
  { id :: String
  , data :: Array a
  , visible :: Boolean
  , opacity :: Number
  , pickable :: Boolean
  , fp64 :: Boolean
  , onHover :: EffectFn1 (PickingInfo a) Boolean
  , onClick :: EffectFn1 (PickingInfo a) Boolean
  , autoHighlight :: Boolean
  , highlightedObjectIndex :: Int
  , highlightColor :: Array Int
  | rest
  }
