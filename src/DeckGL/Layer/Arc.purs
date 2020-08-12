module DeckGL.Layer.Arc where

import DeckGL (Layer)
import DeckGL.BaseProps (BaseProps)
import WebMercator.LngLat (LngLat)

foreign import defaultArcProps :: forall d. ArcLayerProps d
foreign import makeArcLayer :: forall d. ArcLayerProps d -> Layer

type ArcData d = {|d}

type ArcLayerProps d = BaseProps
  ( getSourcePosition :: ArcData d -> LngLat
  , getSourceColor :: Array Int
  , getTargetPosition :: ArcData d -> LngLat
  , getTargetColor :: Array Int
  , strokeWidth :: Int
  , getWidth :: Int
  , getHeight :: Int
  , getTilt :: Int
  , widthMinPixels :: Int 
  )
  (ArcData d)
