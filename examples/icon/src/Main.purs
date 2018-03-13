module Main where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, Json, decodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Either (either)
import Data.Generic (class Generic)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Record.Builder (merge, build)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window as Window
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Network.HTTP.Affjax (get, AJAX)
import MapGL as MapGL
import Partial.Unsafe (unsafePartial)
import React as R
import ReactDOM (render)

main :: forall eff. Eff (dom :: DOM | eff) Unit
main = void  $ elm' >>= render (R.createFactory mapClass unit)
  where
    elm' :: Eff (dom :: DOM | eff) Element
    elm' = do
      win <- window
      doc <- Window.document win
      elm <- getElementById (ElementId "app") (documentToNonElementParentNode (htmlDocumentToDocument doc))
      pure $ unsafePartial (fromJust elm)

-- | Map Component
mapClass :: forall props . R.ReactClass props
mapClass = R.createClass mapSpec

mapSpec ::  forall props eff . R.ReactSpec props MapGL.Viewport R.ReactElement (dom :: DOM | eff)
mapSpec = R.spec' (const initialViewport) render
  where
    render this = do
      let mapProps' = merge { onChangeViewport: mkEffFn1 (void <<< R.writeState this)
                            , onClick: mkEffFn1 (const $ pure unit)
                            , mapStyle: mapStyle
                            , mapboxApiAccessToken: mapboxApiAccessToken
                            }
      vp <- unwrap <$> R.readState this
      let mapProps = build mapProps' vp
      pure $ R.createFactory MapGL.mapGL mapProps

initialViewport :: forall eff. Eff (dom :: DOM | eff) MapGL.Viewport
initialViewport = do
  win <- window
  w <- Window.innerWidth win
  h <- Window.innerHeight win
  pure $
    MapGL.Viewport { width: w
                   , height: h
                   , longitude: -74.00539284665783
                   , latitude: 40.70544878575082
                   , zoom: 10.822714855509464
                   , pitch: 0.0
                   , bearing: 0.0
                   }

-- | Icon Layer Component
type MeteoriteProps =
  { width :: Int
  , height :: Int
  , latitude :: Number
  , longitude :: Number
  , zoom :: Number
  , bearing :: Number
  , pitch :: Number
  , data :: Array Meteorite
  }



newtype Meteorite =
  Mereorite { class :: String
            , coordinates :: Array Number
            , mass :: String
            , name :: String
            , year :: Int
            }

derive instance newtypeMeteorite :: Newtype Meteorite _

derive instance genericMeteorite :: Generic Meteorite

instance decodeJsonMeteorite :: DecodeJson Meteorite where
  decodeJson = gDecodeJson

getMeteoriteData :: forall e . Aff (ajax :: AJAX | e) (Array Meteorite)
getMeteoriteData = do
  (meteorResp :: Json) <-  _.response <$> get meteoritesUrl
  either (throwError <<< error) pure $ decodeJson meteorResp

meteoritesUrl :: String
meteoritesUrl = "https://github.com/uber-common/deck.gl-data/blob/master/examples/icon/meteorites.json"

mapStyle :: String
mapStyle = "mapbox://styles/mapbox/dark-v9"

mapboxApiAccessToken :: String
mapboxApiAccessToken = "pk.eyJ1IjoiYmxpbmt5MzcxMyIsImEiOiJjamVvcXZtbGYwMXgzMzNwN2JlNGhuMHduIn0.ue2IR6wHG8b9eUoSfPhTuQ"
