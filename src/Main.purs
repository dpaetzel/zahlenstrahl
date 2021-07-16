module Main where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array
  ( cons
  , deleteAt
  , length
  , modifyAt
  , snoc
  , updateAt
  , zipWith
  , (..)
  )
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
  , Step
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
  | ChangeStepWidth Int Number
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
    initialState =
      const { numLine : defNumberLine, dataURL : Nothing }

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
      , mkRow' [ BS.mb5 ]
        [ mkColumn BS.col3 $
          mkSettingsInputs state N.fromString settings
        , mkColumn BS.col1 []
        , mkColumn BS.col4 <<< pure $
          HH.table
          [ HP.classes [ BS.table, BS.tableStriped ] ]
          [ annotationsHeader
          , HH.tbody_ $ mkAnnotationInputs state
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
  ChangeStart start ->
    H.modify_ \st -> setStartEnd st ({ start : start, end : st.numLine.end })
  ChangeEnd end -> do
    H.modify_ \st -> setStartEnd st ({ start : st.numLine.start, end : end })
  ChangeStepWidth i s -> do
    H.modify_ \state ->
      case modifyAt i (_ { width = s }) state.numLine.steps of
        Just steps -> state { numLine { steps = steps } }
        Nothing -> state
    H.modify_ fixSteps
  -- TODO Make steps toggleable
  ChangeWidth w ->
    H.modify_ \state -> state { numLine { canvas { width = w } } }
  ChangeHeight h ->
    H.modify_ \state -> state { numLine { canvas { height = h } } }

-- | Set start and end while adjusting all step sizes so the scale stays the
-- | same (but for its labels).
setStartEnd
  :: forall r.
     State
  -> { start :: Number, end :: Number | r }
  -> State
setStartEnd stOld startend@{ start, end } =
  stOld
  { numLine
    { start = start
    , end = end
    , steps =
      map
        (scaleStep stOld.numLine startend)
        stOld.numLine.steps
    }
  }

-- | widthOld / (endOld - startOld) = widthNew / (endNew - startNew)
-- | widthNew = widthOld / (endOld - startOld) (endNew - startNew )
scaleStep
  :: forall r p.
     { start :: Number, end :: Number | r }
  -> { start :: Number, end :: Number | p }
  -> Step
  -> Step
scaleStep iOld iNew s =
  s { width = s.width / (iOld.end - iOld.start) * (iNew.end - iNew.start) }

fixSteps :: State -> State
fixSteps st =
  st { numLine { steps = map (fixStep st.numLine) st.numLine.steps } }

fixStep
  :: forall r p.
     { start :: Number
     , end :: Number
     , canvas :: { width :: Int | p } | r }
  -> Step
  -> Step
fixStep nl s =
  s { width = max s.width (minimumStep nl) }
  where
    minStep = minimumStep nl

-- | (end - start) / step <= width
-- | (end - start) <= width * step
-- | step >= (end - start) / width
minimumStep
  :: forall r p.
     { start :: Number
     , end :: Number
     , canvas :: { width :: Int | p } | r }
  -> Number
minimumStep { start, end, canvas } = (end - start) / I.toNumber canvas.width

mkCanvas :: forall w i. Canvas -> HH.HTML w i
mkCanvas cv =
  HH.canvas
    [ HP.id canvasID
    , HP.width cv.width
    , HP.height cv.height
    , HP.style $ "width: " <> show cv.width <> "px"
    , HP.classes [ BS.mxAuto ]
    ]

mkAnnotationInputs :: forall m. State -> Array (HH.ComponentHTML Action () m)
mkAnnotationInputs state = zipWith
  mkAnnotationInput
  (0..(length state.numLine.annotations))
  state.numLine.annotations

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
mkSettingsInputs st read setts =
  map (mkSettingsInput st read) setts <> mkStepInputs st

mkStepInputs :: forall m. State -> Array (HH.ComponentHTML Action () m)
mkStepInputs state = zipWith
  mkStepInput
  (0..(length state.numLine.steps))
  state.numLine.steps

mkStepInput
  :: forall m.
     Int -> Step -> HH.ComponentHTML Action () m
mkStepInput i { name , width , tickLength , labelled } =
  -- TODO Refactor, the following is structurally the same as mkSettingsInput
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
      [ HH.text name ]
    ]
  , HH.input
    [ HP.classes [ BS.formControl ]
    , HP.value (show width)
    , HE.onValueChange \s ->
       ChangeStepWidth i (fromMaybe width $ N.fromString s)
    ]
  ]

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

mkErrorMsg
  :: forall m a.
     Maybe String
  -> HH.ComponentHTML Action () m
mkErrorMsg error =
  case error of
    Nothing -> HH.div [] []
    Just err ->
      HH.div
      [ HP.classes
        [ BS.m2, BS.w100, BS.alert, BS.alertDanger ]
      ]
      [ HH.text err ]

mkRow :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
mkRow = HH.div [ HP.classes [ BS.row ] ]

mkRow' :: forall w i. Array HH.ClassName -> Array (HH.HTML w i) -> HH.HTML w i
mkRow' cls = HH.div [ HP.classes $ BS.row `cons` cls ]

mkColumn :: forall w i. HH.ClassName -> Array (HH.HTML w i) -> HH.HTML w i
mkColumn cls = HH.div [ HP.classes [ cls ] ]

mkColumn' :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
mkColumn' = HH.div [ HP.classes [ BS.col ] ]
