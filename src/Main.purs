module Main where

import Prelude

import Data.Array (cons, deleteAt, length, snoc, updateAt, zipWith, (..))
import Data.Number as N
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Graphics.Canvas as C
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
-- import DOM.HTML.Indexed as HI
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafePartial)

import Graphics.Zahlengerade (Annotation, Canvas, NumberLine, annotation, clearCanvas, defNumberLine, drawNumberLine, strokeCanvas)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type Input = Unit

type State = NumberLine

data Action
  = Add
  | Edit Int Annotation
  | Remove Int
  | ChangeStart Number
  | ChangeEnd Number
  | ChangeStep Number
  | ChangeMediumStep Number
  | ChangeMiniStep Number

component
  :: forall query output m.
     MonadEffect m => H.Component query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval : H.mkEval $ H.defaultEval { handleAction = handleAction' }
    }
  where
    initialState = const defNumberLine

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_ $
    [ HH.div_ $
      [ HH.h2_ [ HH.text "Zahlenbereich" ] ]
      <> mkSettingsInput "Beginn" state.start ChangeStart
      <> mkSettingsInput "Ende" state.end ChangeEnd
    ]
    <>
    [ HH.div_ $
      [ HH.h2_ [ HH.text "Skala" ] ]
      <> mkSettingsInput "Schritt" state.step ChangeStep
      <> mkSettingsInput "Mittelschritt" state.mediumStep ChangeMediumStep
      <> mkSettingsInput "Minischritt" state.miniStep ChangeMiniStep
    ]
    <>
    [ HH.div_ $
      [ HH.h2_ [ HH.text "Markierungen" ] ]
      `snoc` HH.table_ (annotationsHeader `cons` annotations `snoc` addButton)
    ]
    <>
    [ HH.div_ [ mkCanvas defCanvas ]
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
        (0..(length state.annotations))
        state.annotations
      addButton =
        HH.tr_
        [ HH.td_ []
        , HH.td_ []
        , HH.td_
          [ HH.button
            [ HE.onClick \_ -> Add
            , HP.classes [ HH.ClassName "fa fa-plus"]
            ]
            []
          ]
        ]


canvasID :: String
canvasID = "canvas"

defCanvas :: Canvas
defCanvas = { width : 1000, height : 200 }

handleAction'
  :: forall output m.
     MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction' action = do
  handleAction action
  state <- H.get
  let cv = defCanvas
  H.liftEffect $ void $ unsafePartial do
    Just canvas <- C.getCanvasElementById canvasID
    ctx <- C.getContext2D canvas

    C.beginPath ctx

    clearCanvas ctx cv

    drawNumberLine ctx cv state

    strokeCanvas ctx 2.0 cv

    C.stroke ctx

    -- TODO Add download support. This probably requires a component output and
    -- another component has to listen to that and display a download button.
    -- url <- C.canvasToDataURL canvas


handleAction
  :: forall output m.
     Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Add ->
    H.modify_ \state ->
      state { annotations = snoc state.annotations annotation }
  Edit i a ->
    H.modify_ \state ->
      case updateAt i a state.annotations of
        Just annotations -> state { annotations = annotations }
        Nothing -> state
  Remove i ->
    H.modify_ \state ->
      case deleteAt i state.annotations of
        Just annotations -> state { annotations = annotations }
        Nothing -> state
  ChangeStart s ->
    H.modify_ \state -> state { start = s }
  ChangeEnd s ->
    H.modify_ \state -> state { end = s }
  ChangeStep s ->
    H.modify_ \state -> state { step = s }
  ChangeMediumStep s ->
    H.modify_ \state -> state { mediumStep = s }
  ChangeMiniStep s ->
    H.modify_ \state -> state { miniStep = s }
  -- TODO Make steps toggleable


mkCanvas :: forall w i. Canvas -> HH.HTML w i
mkCanvas cv =
  HH.canvas [ HP.id canvasID, HP.width cv.width, HP.height cv.height ]

{-
i is index, a is annotation.

-}
mkAnnotationInput
  :: forall m.
     Int -> Annotation -> HH.ComponentHTML Action () m
mkAnnotationInput i a =
  HH.tr_
    [ HH.td_
      [ HH.input
        [ HP.value (show a.place)
        , HE.onValueChange \p ->
           Edit i a { place = fromMaybe a.place $ N.fromString p }
        ]
      ]
    , HH.td_
      [ HH.input
        [ HP.value a.label
        , HE.onValueChange \l ->
        Edit i a { label = l }
        ]
      ]
    , HH.td_
      [ HH.button
        [ HE.onClick \_ -> Remove i
        , HP.classes [ HH.ClassName "fa fa-minus"]
        ]
        []
      ]
    ]

mkSettingsInput
  :: forall m.
     String -> Number -> (Number -> Action) ->
     Array (HH.ComponentHTML Action () m)
mkSettingsInput label oldVal action =
  [ HH.label [ HP.for label ] [ HH.text $ label <> ":" ]
  , HH.input
    [ HP.name label
    , HP.value (show oldVal)
    , HE.onValueChange \s -> action (fromMaybe oldVal $ N.fromString s)
    ]
  ]
