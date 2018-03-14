module Main where

import Prelude
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (execState, State, modify, get)
import Data.Argonaut as A
import Data.Array ((!!), (..), length, filter, foldl)
import Data.Either (either)
import Data.Int (floor, toNumber)
import Data.Maybe (fromJust, fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set as S
import Data.StrMap as StrMap
import Data.Map as Map
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import DeckGL as DeckGL
import DeckGL.Layer.Icon as Icon
import DeckGL.Projection (makeMercatorProjector, project, getBoundingBox)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window as Window
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import MapGL as MapGL
import Math (pow)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax (affjax, defaultRequest, get) as Affjax
import Network.HTTP.RequestHeader (RequestHeader(..))
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

mapSpec ::  forall props eff . R.ReactSpec props MapState R.ReactElement (dom :: DOM, ajax :: AJAX | eff)
mapSpec = (R.spec' getInitialState render) {componentWillMount = onComponentWillMount}
  where
    render this = do
      state <- R.readState this
      let viewport@(MapGL.Viewport vp) = state.viewport
          relevantMeteorites = getMeteoritesInBoundingBox viewport state.data
          mapProps = { onChangeViewport: mkEffFn1 \newVp -> void $ R.transformState this _{viewport = newVp}
                     , onClick: mkEffFn1 (const $ pure unit)
                     , mapStyle: mapStyle
                     , mapboxApiAccessToken: mapboxApiAccessToken
                     , width: vp.width
                     , height: vp.height
                     , latitude: vp.latitude
                     , longitude: vp.longitude
                     , zoom: vp.zoom
                     , bearing: vp.bearing
                     , pitch: vp.pitch
                     }
          overlayProps = {viewport, data: relevantMeteorites, iconMapping: state.iconMapping, iconAtlas: state.iconAtlas}
      pure $ R.createElement MapGL.mapGL mapProps [R.createFactory iconLayerClass overlayProps]

    getInitialState :: forall eff'. R.GetInitialState props MapState (dom :: DOM | eff')
    getInitialState this = do
      vp <- initialViewport
      pure $ { viewport: vp
             , iconAtlas: iconAtlasUrl
             , iconMapping: StrMap.empty
             , data: []
             }

    onComponentWillMount :: forall eff'. R.ComponentWillMount props MapState (ajax :: AJAX | eff')
    onComponentWillMount this = void <<< launchAff $ do
      iconMapping <- buildIconMapping
      meteorites <- getMeteoriteData
      void $ liftEff $ R.transformState this _{ iconMapping = iconMapping
                                              , data = meteorites
                                              }

    getMeteoritesInBoundingBox :: MapGL.Viewport -> Array Meteorite -> Array Meteorite
    getMeteoritesInBoundingBox vp ms =
      let boundingBox = getBoundingBox vp
          isInBox m = let lngLat = meteoriteLngLat m
                      in    MapGL.lng lngLat <= boundingBox.ne.lng
                         && MapGL.lng lngLat >= boundingBox.sw.lng
                         && MapGL.lat lngLat <= boundingBox.ne.lat
                         && MapGL.lat lngLat >= boundingBox.sw.lat
      in filter isInBox ms

type MapState =
  { viewport :: MapGL.Viewport
  , iconAtlas :: String
  , iconMapping :: Icon.IconMapping
  , data :: Array Meteorite
  }

-- | Get the initial viewport based on the window dimensions.
initialViewport :: forall eff. Eff (dom :: DOM | eff) MapGL.Viewport
initialViewport = do
  win <- window
  w <- Window.innerWidth win
  h <- Window.innerHeight win
  pure $
    MapGL.Viewport { width: w
                   , height: h
                   , longitude: -35.0
                   , latitude: 36.7
                   , zoom: 1.8
                   , pitch: 0.0
                   , bearing: 0.0
                   }

-- | IconMapping entry
newtype IconEntry =
  IconEntry { label :: String
            , x :: Int
            , y :: Int
            , width :: Int
            , height :: Int
            , anchorY :: Int
            }

instance decodeJsonIconEntry :: A.DecodeJson IconEntry where
  decodeJson json = do
    obj <- A.decodeJson json
    label <- obj A..? "label"
    x <- obj A..? "x"
    y <- obj A..? "y"
    width <- obj A..? "width"
    height <- obj A..? "height"
    anchorY <- obj A..? "anchorY"
    pure $ IconEntry {label, x, y, width, height, anchorY}

-- | Make a request to the data directory to make the `IconMapping`, which is just a mapping
-- | from label name (e.g. marker-1) to the section of the imageAtlas which you can find the
-- | right icon.
buildIconMapping :: forall eff. Aff (ajax :: AJAX | eff) Icon.IconMapping
buildIconMapping = do
    (mappingResp :: A.Json) <-  _.response <$> Affjax.get iconUrl
    (icons :: Array IconEntry) <- either (throwError <<< error) pure $ A.decodeJson mappingResp
    pure $ foldl (\mapping icon -> StrMap.insert (makeLabel icon) (makeEntry icon) mapping) StrMap.empty icons
  where
    makeLabel (IconEntry icon) = icon.label
    makeEntry (IconEntry icon) =
      { x: icon.x
      , y: icon.y
      , width: icon.width
      , height: icon.height
      , mask: false
      }

--------------------------------------------------------------------------------
-- | DeckGL Component
--------------------------------------------------------------------------------

-- | Icon Layer Component
type MeteoriteProps =
  { viewport :: MapGL.Viewport
  , data :: Array Meteorite
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
          meteoriteDate = flip map $ \m -> {meteorite: m}
          iconLayer = Icon.makeIconLayer $
                        ( Icon.defaultIconProps { id = "icon"
                                                , data = map (\m -> {meteorite : m}) props.data
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
      let updatedCluster = updateCluster props
      pure $ updatedCluster


    receiveProps :: R.ComponentWillReceiveProps MeteoriteProps MeteoriteState eff
    receiveProps this newProps = do
      currentProps <- R.getProps this
      if map meteoriteId newProps.data /= map meteoriteId currentProps.data
        then let newZL = updateCluster newProps
             in void $ R.writeState this newZL
        else pure unit

updateCluster :: MeteoriteProps -> {zoomLevels :: ZoomLevels}
updateCluster props =
    let vp = unwrap props.viewport
        vpZoomedOut = wrap $ vp {zoom = 0.0}
        mp = makeMercatorProjector vpZoomedOut
        bush = RBush.empty 5
        screenData = flip map props.data $ \d ->
          let lngLat = meteoriteLngLat d
              sCoords = project mp lngLat
          in { entry: d
             , x: toNumber sCoords.x
             , y: toNumber sCoords.y
             }
        fullBush = RBush.insertMany screenData bush
    in {zoomLevels: fillOutZoomLevels screenData fullBush}

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
    if entryZoomHash entry `S.member` known
       then pure unit
       else let box = { minX: x - radius
                      , minY: y - radius
                      , maxX: x + radius
                      , maxY: y + radius
                      }
                allNeighbors = RBush.search box bush
                newNeighbors = filter (\n -> not $ entryZoomHash n.entry `S.member` known) allNeighbors
            in for_ newNeighbors $ \node ->
                 let nodeId = meteoriteId node.entry
                 in if nodeId == meteoriteId entry
                      then modify \s -> s { zoomLevels = Map.insert (Tuple nodeId zoom) { icon: getIconName $ length newNeighbors
                                                                                        , size: getIconSize $ length newNeighbors
                                                                                        } s.zoomLevels
                                          , knownSet = S.insert (entryZoomHash node.entry) s.knownSet
                                          }
                      else modify \s -> s { knownSet = S.insert (entryZoomHash node.entry) s.knownSet
                                          }
  where
    entryZoomHash e = meteoriteId e <> show zoom
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

derive instance newtypeMeteorite :: Newtype Meteorite _

instance decodeJsonMeteorite :: A.DecodeJson Meteorite where
  decodeJson json = do
    obj <- A.decodeJson json
    _class <- obj A..? "class"
    coordinates <- obj A..? "coordinates"
    mass <- obj A..? "mass"
    name <- obj A..? "name"
    year <- obj A..? "year"
    pure $ Meteorite {class: _class, coordinates, mass, name, year}

-- | meteoriteId is effectively a hash of a meteorite.
meteoriteId :: Meteorite -> String
meteoriteId (Meteorite m) =
  m.class <> show m.coordinates <> m.mass <> m.name <> show m.year

-- | Convert the coordinates of a meteorite into LngLat
meteoriteLngLat :: Meteorite -> MapGL.LngLat
meteoriteLngLat (Meteorite m)= unsafePartial fromJust $ do
  x <- m.coordinates !! 0
  y <- m.coordinates !! 1
  pure $ MapGL.makeLngLat x y

-- | Fetch the meteorite data from the data directory.
getMeteoriteData :: forall e . Aff (ajax :: AJAX | e) (Array Meteorite)
getMeteoriteData = do
  let req = Affjax.defaultRequest { url = meteoritesUrl
                                  , headers = [ RequestHeader "Access-Control-Allow-Origin" "*"
                                              , RequestHeader "Contenty-Type" "application/json"
                                              ]
                                  }
  (meteorResp :: A.Json) <-  _.response <$> Affjax.affjax req
  either (throwError <<< error) pure $ A.decodeJson meteorResp

--------------------------------------------------------------------------------
-- | Utils and Config
--------------------------------------------------------------------------------

-- | The icon's name is based on the size of the cluster.
getIconName :: Int -> String
getIconName size
  | size == 0 = ""
  | size < 10 = "marker-" <> show size
  | size < 100 = "marker-" <> show (size / 10) <> "0"
  | otherwise = "marker-100"

-- | Scale the icon according to the size of the cluster.
getIconSize :: Int -> Number
getIconSize size = (min 100.0 (toNumber size) / 50.0) + 0.5

-- | The base icon size.
iconSize :: Number
iconSize = 60.0


-- | data directory urls.
meteoritesUrl :: String
meteoritesUrl = "data/meteorites.json"

iconAtlasUrl :: String
iconAtlasUrl = "data/location-icon-atlas.png"

iconUrl :: String
iconUrl = "data/location-icon-mapping.json"


mapStyle :: String
mapStyle = "mapbox://styles/mapbox/dark-v9"

mapboxApiAccessToken :: String
mapboxApiAccessToken = "pk.eyJ1IjoiYmxpbmt5MzcxMyIsImEiOiJjamVvcXZtbGYwMXgzMzNwN2JlNGhuMHduIn0.ue2IR6wHG8b9eUoSfPhTuQ"
