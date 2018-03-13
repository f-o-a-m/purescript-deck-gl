module Main where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (execState, State, modify, get)
import Data.Argonaut (class DecodeJson, Json, decodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Array ((!!), (..), length, filter)
import Data.Either (either)
import Data.Int (floor, toNumber)
import Data.Generic (class Generic)
import Data.Maybe (fromJust, fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set as S
import Data.Map as Map
import Data.Record.Builder (merge, build)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import DeckGL as DeckGL
import DeckGL.Layer.Icon as Icon
import DeckGL.Projection (makeMercatorProjector, project)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window as Window
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import MapGL as MapGL
import Math (pow)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax (get) as AffJax
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

--------------------------------------------------------------------------------
-- | Map Component
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- | DeckGL Component
--------------------------------------------------------------------------------

-- | Icon Layer Component
type MeteoriteProps =
  { viewport :: MapGL.Viewport
  , data :: Array (Icon.IconData (meteorite :: Meteorite))
  , iconMapping :: Icon.IconMapping
  , iconAtlas :: String
  }

type MeteoriteState =
  { zoomLevels :: ZoomLevels
  }

iconLayerClass :: R.ReactClass MeteoriteProps
iconLayerClass = R.createClass iconLayerSpec

iconLayerSpec :: forall eff . R.ReactSpec MeteoriteProps MeteoriteState R.ReactElement eff
iconLayerSpec = (R.spec' getInitialState render) {componentWillReceiveProps = receiveProps}
  where
    render this = do
      props <- R.getProps this
      state <- R.readState this
      let vp = unwrap props.viewport
          currentZoom = floor vp.zoom
          iconLayer = Icon.makeIconLayer $
                        ( Icon.defaultIconProps { id = "icon"
                                                , data = props.data
                                                , pickable = false
                                                , visible = true
                                                , iconAtlas = props.iconAtlas
                                                , iconMapping = props.iconMapping
                                                , sizeScale = 2.0 * iconSize
                                                , getPosition = \{meteorite} -> meteoriteLngLat meteorite
                                                , getIcon = \{meteorite} ->
                                                    let mId = meteoriteId meteorite
                                                    in fromMaybe "marker" (_.icon <$> Map.lookup (Tuple mId currentZoom) state.zoomLevels)
                                                , getSize = \{meteorite} ->
                                                    let mId = meteoriteId meteorite
                                                    in fromMaybe 1.0 (_.size <$> Map.lookup (Tuple mId currentZoom) state.zoomLevels)

                                                , autoHighlight = true
                                                , highlightedObjectIndex = 0
                                                })
      pure $ R.createFactory DeckGL.deckGL { layers: [iconLayer]
                                           , initializer: DeckGL.initializeGL
                                           , zoom: vp.zoom
                                           , width: vp.width
                                           , height: vp.height
                                           , latitude: vp.latitude
                                           , longitude: vp.longitude
                                           , pitch: vp.pitch
                                           , bearing: vp.bearing
                                           }

    getInitialState ::  R.GetInitialState MeteoriteProps MeteoriteState eff
    getInitialState this = do
      props <- R.getProps this
      pure $ updateCluster props

    receiveProps :: R.ComponentWillReceiveProps MeteoriteProps MeteoriteState eff
    receiveProps this newProps = do
      currentProps <- R.getProps this
      let oldViewport = unwrap currentProps.viewport
          newViewport = unwrap newProps.viewport
          getMeteoriteIds = map $ \d -> meteoriteId d.meteorite
      if   getMeteoriteIds newProps.data /= getMeteoriteIds currentProps.data
        || oldViewport.width /= newViewport.width
        ||  newViewport.height /= oldViewport.height
        then let newZL = updateCluster newProps
             in void $ R.writeState this newZL
        else pure unit



updateCluster :: MeteoriteProps -> {zoomLevels :: ZoomLevels}
updateCluster props =
    let vpZoomedOut = wrap $ (unwrap props.viewport) {zoom = 0.0}
        mp = makeMercatorProjector vpZoomedOut
        bush = RBush.empty 5
        screenData = flip map props.data $ \d ->
          let lngLat = getLngLat d.meteorite
              sCoords = project mp lngLat
          in { entry: d.meteorite
             , x: toNumber sCoords.x
             , y: toNumber sCoords.y
             }
        fullBush = RBush.insertMany screenData bush
    in {zoomLevels: fillOutZoomLevels screenData fullBush}
  where
    getLngLat :: Meteorite -> MapGL.LngLat
    getLngLat (Meteorite m) = unsafePartial fromJust $ do
      lng <- m.coordinates !! 0
      lat <- m.coordinates !! 1
      pure $ MapGL.makeLngLat lng lat


--------------------------------------------------------------------------------
-- | ZoomLevels
--------------------------------------------------------------------------------

type ZoomLevelData =
  { icon :: String
  , size :: Number
  }

-- | For a given Meteorite (with meteoriteId) and zoom level n
-- | return the zoom level data, if there is any
type ZoomLevels = Map.Map (Tuple String Int) ZoomLevelData

type ZoomLevelState = {knownSet :: S.Set String, zoomLevels :: ZoomLevels}

fillOutZoomLevel
  :: Array (RBush.Node Meteorite)
  -> Int
  -> ReaderT (RBush.RBush Meteorite) (State ZoomLevelState) Unit
fillOutZoomLevel ms zoom = for_ ms $ \{x, y, entry} -> do
    bush <- ask
    known <- _.knownSet <$> get
    if meteoriteId entry `S.member` known
       then pure unit
       else let box = { minX: x - radius
                      , minY: y - radius
                      , maxX: x + radius
                      , maxY: y + radius
                      }
                allNeighbors = RBush.search box bush
                newNeighbors = filter (\n -> not $ meteoriteId n.entry `S.member` known) allNeighbors
            in for_ newNeighbors $ \node ->
                 let nodeId = meteoriteId node.entry
                 in if nodeId == meteoriteId entry
                      then modify \s -> s { zoomLevels = Map.insert (Tuple nodeId zoom) { icon: getIconName $ length newNeighbors
                                                                                        , size: getIconSize $ length newNeighbors
                                                                                        } s.zoomLevels
                                          , knownSet = S.insert nodeId s.knownSet
                                          }
                      else modify \s -> s { knownSet = S.insert nodeId s.knownSet
                                          }
  where
    radius = iconSize / (2.0 `pow` toNumber (zoom + 1))

fillOutZoomLevels
  :: Array (RBush.Node Meteorite)
  -> RBush.RBush Meteorite
  -> ZoomLevels
fillOutZoomLevels nodes bush =
  let initialState = {knownSet: S.empty, zoomLevels: Map.empty}
      buildZoomLevelsMap = for_ (0 .. 20) (fillOutZoomLevel nodes)
  in _.zoomLevels $ execState (runReaderT buildZoomLevelsMap bush) initialState


--------------------------------------------------------------------------------
-- | Meteorite
--------------------------------------------------------------------------------

newtype Meteorite =
  Meteorite { class :: String
            , coordinates :: Array Number
            , mass :: String
            , name :: String
            , year :: Int
            }

meteoriteId :: Meteorite -> String
meteoriteId (Meteorite m) =
  m.class <> show m.coordinates <> m.mass <> m.name <> show m.year

meteoriteLngLat :: Meteorite -> MapGL.LngLat
meteoriteLngLat (Meteorite m)= unsafePartial fromJust $ do
  x <- m.coordinates !! 0
  y <- m.coordinates !! 1
  pure $ MapGL.makeLngLat x y

derive instance newtypeMeteorite :: Newtype Meteorite _

derive instance genericMeteorite :: Generic Meteorite

instance eqMeteorite :: Eq Meteorite where
  eq a b = meteoriteId a == meteoriteId b

instance decodeJsonMeteorite :: DecodeJson Meteorite where
  decodeJson = gDecodeJson

getMeteoriteData :: forall e . Aff (ajax :: AJAX | e) (Array Meteorite)
getMeteoriteData = do
  (meteorResp :: Json) <-  _.response <$> AffJax.get meteoritesUrl
  either (throwError <<< error) pure $ decodeJson meteorResp

--------------------------------------------------------------------------------
-- | Utils and Config
--------------------------------------------------------------------------------

getIconName :: Int -> String
getIconName size
  | size == 0 = ""
  | size < 10 = "marker-" <> show size
  | size < 100 = "marker-" <> show (size / 10) <> "0"
  | otherwise = "marker-100"

getIconSize :: Int -> Number
getIconSize size = (min 100.0 (toNumber size) / 50.0) + 0.5

iconSize :: Number
iconSize = 60.0

meteoritesUrl :: String
meteoritesUrl = "https://github.com/uber-common/deck.gl-data/blob/master/examples/icon/meteorites.json"

mapStyle :: String
mapStyle = "mapbox://styles/mapbox/dark-v9"

mapboxApiAccessToken :: String
mapboxApiAccessToken = "pk.eyJ1IjoiYmxpbmt5MzcxMyIsImEiOiJjamVvcXZtbGYwMXgzMzNwN2JlNGhuMHduIn0.ue2IR6wHG8b9eUoSfPhTuQ"
