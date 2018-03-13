module Main where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, Json, decodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Array ((!!))
import Data.Either (either)
import Data.Int (toNumber)
import Data.Generic (class Generic)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Record.Builder (merge, build)
import DeckGL.Projection (makeMercatorProjector)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window as Window
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Network.HTTP.Affjax (get, AJAX)
import MapGL as MapGL
import Partial.Unsafe (unsafePartial)
import RBush as RBush
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

type MeteoriteState =
  { meteorites :: Array Meteorite
  , viewport :: MapGL.Viewport
  }

updateCluster :: forall props eff.
                 R.ReactThis props MeteoriteState
              -> Eff (state :: R.ReactState R.ReadWrite | eff) Unit
updateCluster this = do
    st <- R.readState this
    let vpZoomedOut = wrap $ (unwrap st.viewport) {zoom = 0.0}
        mp = makeMercatorProjector vpZoomedOut
        bush = RBush.empty
  --      screenData = flip map st.meteorites $ \m ->
  --        let lngLat = unsafePartialMapGL.makeLngLat 
    pure unit
  where
    getLngLat :: Meteorite -> MapGL.LngLat
    getLngLat (Meteorite m) = unsafePartial fromJust $ do
      lng <- m.coordinates !! 0
      lat <- m.coordinates !! 1
      pure $ MapGL.makeLngLat lng lat

newtype Meteorite =
  Meteorite { class :: String
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

-- | Utils

getIconName :: Int -> String
getIconName size
  | size == 0 = ""
  | size < 10 = "marker-" <> show size
  | size < 100 = "marker-" <> show (size / 10) <> "0"
  | otherwise = "marker-100"

getIconSize :: Int -> Number
getIconSize size = (min 100.0 (toNumber size) / 50.0) + 0.5


-- | Config

meteoritesUrl :: String
meteoritesUrl = "https://github.com/uber-common/deck.gl-data/blob/master/examples/icon/meteorites.json"

mapStyle :: String
mapStyle = "mapbox://styles/mapbox/dark-v9"

mapboxApiAccessToken :: String
mapboxApiAccessToken = "pk.eyJ1IjoiYmxpbmt5MzcxMyIsImEiOiJjamVvcXZtbGYwMXgzMzNwN2JlNGhuMHduIn0.ue2IR6wHG8b9eUoSfPhTuQ"
