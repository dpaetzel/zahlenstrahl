module Main where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array (deleteAt, length, snoc, updateAt, zipWith, (..))
import Data.Int as I
import Data.Number as N
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.Vector.Polymorphic ((><))
import Effect.Aff.Class (class MonadAff)
import Effect.Aff as Aff
import Effect (Effect)
import Effect.Exception (throw)
import Graphics.CanvasAction as CA
import Graphics.CanvasAction.CSSOM (devicePixelRatio)
import Graphics.ScaleForDPR as DPR
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Halogen.Themes.Bootstrap4 as BS
import Halogen.Subscription as HS
import Web.DOM.ParentNode (QuerySelector(..))

import Graphics.Zahlengerade
  ( Annotation
  , Canvas
  , NumberLine
  , defAnnotation
  , defCanvas
  , defNumberLine
  , drawNumberLine
  )

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  anchorM <- HA.selectElement (QuerySelector "#app")
  case anchorM of
    Nothing -> do
      body <- HA.awaitBody
      runUI component unit body
    Just anchor ->
      runUI component unit anchor

type Input = Unit

type State =
  { numLine :: NumberLine
  , dataURL :: Maybe String
  }

data Action
  = Initialize
  | Refresh
  | Add
  | Edit Int Annotation
  | Remove Int
  | ChangeStart Number
  | ChangeEnd Number
  | ChangeStep Number
  | ChangeMediumStep Number
  | ChangeMiniStep Number
  | ChangeWidth Int
  | ChangeHeight Int

type Setting a =
  { label :: String
  , accessor :: State -> a
  , action :: a -> Action
  }

settings :: Array (Setting Number)
settings =
  [ { label : "Beginn"        , accessor : _.numLine.start         , action : ChangeStart }
  , { label : "Ende"          , accessor : _.numLine.end           , action : ChangeEnd }
  , { label : "Schritt"       , accessor : _.numLine.step          , action : ChangeStep }
  , { label : "Mittelschritt" , accessor : _.numLine.mediumStep    , action : ChangeMediumStep }
  , { label : "Minischritt"   , accessor : _.numLine.miniStep      , action : ChangeMiniStep }
  ]

intSettings :: Array (Setting Int)
intSettings =
  [ { label : "Breite (px)"   , accessor : _.numLine.canvas.width  , action : ChangeWidth }
  , { label : "Höhe (px)"     , accessor : _.numLine.canvas.height , action : ChangeHeight }
  ]

component
  :: forall query output m. MonadAff m
  => H.Component query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval : H.mkEval $ H.defaultEval
      { handleAction = handleAction'
      , initialize = Just Initialize
      }
    }
  where
    initialState = const { numLine : defNumberLine, dataURL : Nothing }

doc :: forall w i. HH.HTML w i
doc =
  HH.div_
  [ HH.h4_ [ HH.text "Tipps" ]
  , HH.ul_
    [ HH.li_
      [ HH.text $
        "Zwischenschritte können unsichtbar gemacht werden, indem "
        <> " ihr Wert auf eine Zahl größer als "
      , HH.em_ [ HH.text "Ende" ]
      , HH.text " gesetzt wird."
      ]
    , HH.li_
      [ HH.text $
        "Für Zwischenschritte mit periodischen Nachkommastellen einfach genug "
        <> "Nachkommastellen angeben (z.B. 0.333333 für Drittel oder "
        <> "0.142857 für Siebtel)."
      ]
    ]
  ]
  where
    mkLi txt = HH.li_ [ HH.text txt ]

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_ $
    [ HH.div_ $
      [ mkRow
        [ mkColumn BS.mxAuto
          [ mkCanvas state.numLine.canvas ]
        ]
      , mkRow
        [ mkColumn BS.mxAuto
          [ downloadButton
          ]
        ]
      , mkRow
        [ mkColumn BS.col3 $
          mkSettingsInputs state N.fromString settings
          -- <> mkSettingsInputs state.numLine I.fromString intSettings
        , mkColumn BS.col1 []
        , mkColumn BS.col4 <<< pure $
          HH.table
          [ HP.classes [ BS.table, BS.tableStriped ] ]
          [ annotationsHeader
          , HH.tbody_ annotations
          , addButton
          ]
        , mkColumn BS.col4 [doc]
        ]
      ]
    ]
    where
      annotationsHeader :: HH.HTML _ _
      annotationsHeader = HH.tr_ [ HH.th_ [ HH.text "Stelle" ]
                                 , HH.th_ [ HH.text "Beschriftung" ]
                                 , HH.th_ []
                                 ]
      annotations :: Array (HH.HTML _ _)
      annotations = zipWith
        mkAnnotationInput
        (0..(length state.numLine.annotations))
        state.numLine.annotations
      addButton =
        HH.tr_
        [ HH.td_ []
        , HH.td_ []
        , HH.td_
          [ HH.button
            [ HE.onClick \_ -> Add
            , HP.classes [ HH.ClassName "fa fa-plus", BS.btn, BS.btnSecondary ]
            ]
            []
          ]
        ]
      downloadButton =
        case state.dataURL of
          Nothing ->
            HH.div
              [ HP.classes
                [ HH.ClassName "fa fa-download"
                , BS.btn
                , BS.btnPrimary
                , BS.disabled
                ]
              ]
              [ HH.text " Download nicht möglich"
              ]
          Just url ->
            HH.a
              [ HP.href url
              , HP.download "zahlenstrahl.png"
              , HP.classes
                [ HH.ClassName "fa fa-download"
                , BS.btn
                , BS.btnPrimary
                , BS.mb5
                ]
              ]
              [ HH.text " Abbildung herunterladen"
              ]

canvasID :: String
canvasID = "canvas"

getCtx :: Effect CA.Context2D
getCtx = CA.getCanvasElementById canvasID >>= case _ of
  Just cv -> CA.getContext2D cv
  Nothing -> throw "No canvas"

handleAction'
  :: forall output m. MonadAff m
  => Action -> H.HalogenM State Action () output m Unit
handleAction' action = do
  handleAction action
  { numLine, dataURL } <- H.get
  urlPNG <- H.liftEffect $ do
    ctx <- getCtx

    CA.runAction ctx $ do
      dpr <- devicePixelRatio
      DPR.scaleForDPROnce
        (I.toNumber defCanvas.width >< I.toNumber defCanvas.height)
        dpr
      CA.clearRectFull
      drawNumberLine numLine

      CA.toDataUrl

  let url = S.replace
            (S.Pattern "image/png")
            (S.Replacement "image/octet-stream")
            urlPNG
  H.modify_ \state ->
    state { dataURL = Just url }

pushDPR
  :: forall m a. MonadAff m
  => HS.Listener Action -> m (Aff.Fiber a)
pushDPR listener = H.liftAff $ do
  Aff.forkAff $ forever do
    _ <- DPR.getDPRChange
    H.liftEffect $ HS.notify listener Refresh

handleAction
  :: forall output m. MonadAff m
  => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Refresh -> pure mempty
  Initialize -> do
    { emitter, listener } <- H.liftEffect HS.create
    void $ H.subscribe emitter
    void $ pushDPR listener
  Add ->
    H.modify_ \state ->
      state
      { numLine
        { annotations = snoc state.numLine.annotations defAnnotation
        }
      }
  Edit i a ->
    H.modify_ \state ->
      case updateAt i a state.numLine.annotations of
        Just annotations -> state { numLine { annotations = annotations } }
        Nothing -> state
  Remove i ->
    H.modify_ \state ->
      case deleteAt i state.numLine.annotations of
        Just annotations -> state { numLine { annotations = annotations } }
        Nothing -> state
  ChangeStart s ->
    H.modify_ \state -> state { numLine { start = s } }
  ChangeEnd s ->
    -- TODO Add safety check (e.g. if end - start / step is too large, then it hangs)
    H.modify_ \state -> state { numLine { end = s } }
  ChangeStep s ->
    H.modify_ \state -> state { numLine { step = s } }
  ChangeMediumStep s ->
    H.modify_ \state -> state { numLine { mediumStep = s } }
  ChangeMiniStep s ->
    H.modify_ \state -> state { numLine { miniStep = s } }
  -- TODO Make steps toggleable
  ChangeWidth w ->
    H.modify_ \state -> state { numLine { canvas { width = w } } }
  ChangeHeight h ->
    H.modify_ \state -> state { numLine { canvas { height = h } } }

mkCanvas :: forall w i. Canvas -> HH.HTML w i
mkCanvas cv =
  HH.canvas
    [ HP.id canvasID
    , HP.width cv.width
    , HP.height cv.height
    , HP.style $ "width: " <> show cv.width <> "px"
    , HP.classes [ BS.mxAuto ]
    ]

{-
i is index, a is annotation.

-}
mkAnnotationInput
  :: forall m.
     Int -> Annotation -> HH.ComponentHTML Action () m
mkAnnotationInput i a =
  HH.tr_
    [ HH.td_ <<< pure $
      HH.input
        [ HP.classes [ BS.formControl ]
        , HP.value (show a.place)
        , HE.onValueChange \p ->
          Edit i a { place = fromMaybe a.place $ N.fromString p }
        ]
    , HH.td_ <<< pure $
      HH.input
        [ HP.classes [ BS.formControl ]
        , HP.value a.label
        , HE.onValueChange \l ->
          Edit i a { label = l }
        ]
    , HH.td_ <<< pure $
      -- TODO This is <button> but I should use <input type="button">.
      HH.button
        [ HE.onClick \_ -> Remove i
        , HP.classes [ HH.ClassName "fa fa-minus", BS.btn, BS.btnSecondary ]
        ]
        []
    ]

mkSettingsInputs
  :: forall m a.
     Show a =>
     State ->
     (String -> Maybe a) ->
     Array (Setting a) ->
     Array (HH.ComponentHTML Action () m)
mkSettingsInputs st read setts = map (mkSettingsInput st read) setts

mkSettingsInput
  :: forall m a.
     Show a =>
     State ->
     (String -> Maybe a) ->
     Setting a ->
     HH.ComponentHTML Action () m
mkSettingsInput state read setting =
  -- The boilerplate (inputGroup, div nesting, span, etc.) is due to Bootstrap.
  HH.div
  [ HP.classes [ BS.inputGroup, BS.m2 ] ]
  [
    HH.div
    [ HP.classes
      -- The w50 (in combination with the w100) make the labels all the same
      -- width.
      [ BS.inputGroupPrepend, BS.w50 ]
    ]
    [ HH.span [ HP.classes [ BS.inputGroupText, BS.w100 ] ]
      [ HH.text $ setting.label ]
    ]
  , HH.input
    [ HP.classes [ BS.formControl ]
    , HP.value (show oldVal)
    , HE.onValueChange \s -> setting.action (fromMaybe oldVal $ read s)
    ]
  ]
  where
    oldVal = setting.accessor $ state

mkRow :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
mkRow = HH.div [ HP.classes [ BS.row ] ]

mkColumn :: forall w i. HH.ClassName -> Array (HH.HTML w i) -> HH.HTML w i
mkColumn cls = HH.div [ HP.classes [ cls ] ]

mkColumn' :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
mkColumn' = HH.div [ HP.classes [ BS.col ] ]
