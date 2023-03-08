module Main where

import Prelude

import Affjax (defaultRequest, get, printError, request) as Affjax
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (json)
import Affjax.Web (driver) as AffjaxWeb
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (execState, State, modify, get)
import Data.Argonaut as A
import Data.Array ((!!), length, filter, foldl)
import Data.Either (Either(..), either)
import Data.Int (floor, toNumber)
import Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Number (pow)
import Data.Set as S
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import DeckGL as DeckGL
import DeckGL.Layer.Icon as Icon
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Uncurried (mkEffectFn1)
import Foreign.Object as Object
import MapGL as MapGL
import Partial.Unsafe (unsafePartial)
import RBush as RBush
import React as R
import ReactDOM (render)
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import WebMercator.LngLat (LngLat)
import WebMercator.LngLat as LngLat
import WebMercator.Pixel as Pixel
import WebMercator.Viewport (ViewportR)
import WebMercator.Viewport as Viewport

main :: Effect Unit
main = void $ elm' >>= render (R.createLeafElement mapClass {})
  where
    elm' :: Effect Element
    elm' = do
      win <- window
      doc <- Window.document win
      elm <- getElementById "app" (HTMLDocument.toNonElementParentNode doc)
      pure $ unsafePartial (fromJust elm)

--------------------------------------------------------------------------------
-- | Map Component
--------------------------------------------------------------------------------

mapClass :: R.ReactClass {}
mapClass = R.component "Map" \this -> do
  vp <- initialViewport
  launchAff_ do
    iconMapping <- buildIconMapping
    meteorites <- getMeteoriteData
    liftEffect $ R.modifyState this _
      { iconMapping = iconMapping
      , data = meteorites
      }

  pure
    { render: render this
    , state:
        { viewport: MapGL.Viewport vp
        , iconAtlas: iconAtlasUrl
        , iconMapping: Object.empty
        , data: []
        }
    }
  where
    render this = do
      state <- R.getState this
      let viewport@(MapGL.Viewport vp) = state.viewport
          relevantMeteorites = getMeteoritesInBoundingBox vp state.data

          mapProps = MapGL.mkProps viewport $
            { onViewportChange: mkEffectFn1 \newVp -> void $ R.modifyState this _ { viewport = newVp }
            , onClick: mkEffectFn1 (const $ pure unit)
            , mapStyle: mapStyle
            , mapboxApiAccessToken: mapboxApiAccessToken
            , dragRotate: false
            , onLoad: mempty
            , touchZoom: false
            , touchRotate: false
            }

          overlayProps = { viewport
                         , data: relevantMeteorites
                         , iconMapping: state.iconMapping
                         , iconAtlas: state.iconAtlas
                         , discreteZoom: floor vp.zoom
                         }
      pure $ R.createElement MapGL.mapGL mapProps [R.createLeafElement iconLayerClass overlayProps]

    getMeteoritesInBoundingBox :: Record (ViewportR ()) -> Array Meteorite -> Array Meteorite
    getMeteoritesInBoundingBox vp = filter
      $ Viewport.isInBoundingBox (Viewport.boundingBox $ Viewport.pack vp) <<< meteoriteLngLat

type MapState =
  { viewport :: MapGL.Viewport
  , iconAtlas :: String
  , iconMapping :: Icon.IconMapping
  , data :: Array Meteorite
  }

-- | Get the initial viewport based on the window dimensions.
initialViewport :: Effect (Record (ViewportR ()))
initialViewport = do
  win <- window
  w <- Window.innerWidth win
  h <- Window.innerHeight win
  pure
    { width: toNumber w
    , height: toNumber h
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
    label <- obj A..: "label"
    x <- obj A..: "x"
    y <- obj A..: "y"
    width <- obj A..: "width"
    height <- obj A..: "height"
    anchorY <- obj A..: "anchorY"
    pure $ IconEntry {label, x, y, width, height, anchorY}

-- | Make a request to the data directory to make the `IconMapping`, which is just a mapping
-- | from label name (e.g. marker-1) to the section of the imageAtlas which you can find the
-- | right icon.
buildIconMapping :: Aff Icon.IconMapping
buildIconMapping = do
  resp <- Affjax.get AffjaxWeb.driver json iconUrl
  (icons :: Array IconEntry) <- case resp of
    Left e -> throwError $ error $ Affjax.printError e
    Right {body} -> either (throwError <<< error <<< show) pure $ A.decodeJson body
  pure $ foldl (\mapping icon -> Object.insert (makeLabel icon) (makeEntry icon) mapping) Object.empty icons
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
  , discreteZoom :: Int
  }

type MeteoriteState =
  { zoomLevels :: ZoomLevels
  }

iconLayerClass :: R.ReactClass MeteoriteProps
iconLayerClass = R.component "IconLayer" \this -> do
  props <- R.getProps this
  pure
    { unsafeComponentWillReceiveProps: componentWillReceiveProps this
    , render: render this
    , state: updateCluster props
    }
  where
    render this = do
      props <- R.getProps this
      state <- R.getState this

      let vp = unwrap props.viewport
          iconLayer = Icon.makeIconLayer $
                        ( Icon.defaultIconProps { id = "icon"
                                                , data = map (\m -> { meteorite: m }) props.data
                                                , pickable = false
                                                , visible = true
                                                , iconAtlas = props.iconAtlas
                                                , iconMapping = props.iconMapping
                                                , opacity = 1.0
                                                , sizeScale = 2.0 * iconSize
                                                , getPosition = \{meteorite} -> meteoriteLngLat meteorite
                                                , getIcon = \{meteorite} ->
                                                    let mId = meteoriteId meteorite
                                                    in fromMaybe "marker" (_.icon <$> Map.lookup (Tuple mId props.discreteZoom) (state.zoomLevels))
                                                , getSize = \{meteorite} ->
                                                    let mId = meteoriteId meteorite
                                                    in fromMaybe 1.0 (_.size <$> Map.lookup (Tuple mId props.discreteZoom) state.zoomLevels)

                                                })
      pure
        $ R.createLeafElement DeckGL.deckGL
        $ DeckGL.defaultDeckGLProps { layers = [ iconLayer ], viewState = vp }


    componentWillReceiveProps :: R.ReactThis MeteoriteProps MeteoriteState -> R.ComponentWillReceiveProps MeteoriteProps
    componentWillReceiveProps this newProps = do
      currentProps <- R.getProps this
      if map meteoriteId newProps.data /= map meteoriteId currentProps.data || currentProps.discreteZoom /= newProps.discreteZoom
        then let newZL = updateCluster newProps
             in void $ R.writeState this newZL
        else pure unit

updateCluster :: MeteoriteProps -> {zoomLevels :: ZoomLevels}
updateCluster props =
    let vp = unwrap props.viewport
        vpZoomedOut = vp {zoom = 0.0}
        prj = Viewport.project $ Viewport.pack vpZoomedOut
        bush = RBush.empty 5
        screenData = flip map props.data $ \d ->
          let sCoords = prj $ meteoriteLngLat d
          in { entry: d
             , x: Pixel.x sCoords
             , y: Pixel.y sCoords
             }
        fullBush = RBush.insertMany screenData bush
    in {zoomLevels: fillOutZoomLevels screenData fullBush props.discreteZoom}

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
  -> Int
  -> ZoomLevels
fillOutZoomLevels nodes bush zoom  =
  let initialState = { knownSet: S.empty, zoomLevels: Map.empty }
  in _.zoomLevels $ execState (runReaderT (fillOutZoomLevel nodes zoom) bush) initialState


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
    _class <- obj A..: "class"
    coordinates <- obj A..: "coordinates"
    mass <- obj A..: "mass"
    name <- obj A..: "name"
    year <- obj A..: "year"
    pure $ Meteorite {class: _class, coordinates, mass, name, year}

-- | meteoriteId is effectively a hash of a meteorite.
meteoriteId :: Meteorite -> String
meteoriteId (Meteorite m) =
  m.class <> show m.coordinates <> m.mass <> m.name <> show m.year

-- | Convert the coordinates of a meteorite into LngLat
meteoriteLngLat :: Meteorite -> LngLat
meteoriteLngLat (Meteorite m) = LngLat.make $ unsafePartial fromJust $
  {lng: _, lat: _}
    <$> m.coordinates !! 0
    <*> m.coordinates !! 1

-- | Fetch the meteorite data from the data directory.
getMeteoriteData :: Aff (Array Meteorite)
getMeteoriteData = do
  let req = Affjax.defaultRequest { url = meteoritesUrl
                                  , headers = [ RequestHeader "Access-Control-Allow-Origin" "*"
                                              , RequestHeader "Contenty-Type" "application/json"
                                              ]
                                  , responseFormat = json
                                  }
  resp <- Affjax.request AffjaxWeb.driver req
  case resp of
    Left e -> throwError <<< error $ Affjax.printError e
    Right {body} -> either (throwError <<< error <<< show) pure $ A.decodeJson body

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
