module Main where

import Prelude

import Data.Array (deleteAt, length, snoc, updateAt, zip, (..))
import Data.Int as I
import Data.Number as N
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence_)
import Data.Tuple (uncurry)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Graphics.Canvas (rect, fillPath, setFillStyle, strokePath, getContext2D,
                        getCanvasElementById)
import Graphics.Canvas as C
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Math as M
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type Annotation = { place :: Number, label :: String }

annotation :: Annotation
annotation = { place : 0.0, label : "0" }

type NumberLine =
    { start       :: Number
    , end         :: Number
    , step        :: Number
    , mediumStep  :: Number
    , miniStep    :: Number
    , annotations :: Array Annotation
    -- , size :: Number
    }

numberLine :: Input -> State
numberLine _ =
  { start : 0.0,
    end : 10.0,
    step : 1.0,
    mediumStep : 0.5,
    miniStep : 0.1,
    annotations :
    [ { place : 3.14, label : "π"}
    , { place : 2.71, label : "e"}
    ]
  }

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

component :: forall query output m. MonadEffect m => H.Component query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval : H.mkEval $ H.defaultEval { handleAction = handleAction' }
    }
  where
    initialState = numberLine

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_ $
    [ HH.div_
      [ HH.text "Start", mkSettingsInput state.start ChangeStart
      , HH.text "Ende", mkSettingsInput state.end ChangeEnd
      ]
    , HH.div_ [ HH.text "Step", mkSettingsInput state.step ChangeStep ]
    , HH.div_
      [ HH.text "Medium step", mkSettingsInput state.mediumStep ChangeMediumStep
      , HH.text "Mini step", mkSettingsInput state.miniStep ChangeMiniStep
      ]
    ]
    <> annotations
    <>
    [ HH.button [ HE.onClick \_ -> Add ] [ HH.text "+" ]
    , HH.div_ [ HH.text $ show state ]
    , HH.div_ [ canvas' ]
    ]
    where
      annotations = map
        (uncurry mkAnnotationInput)
        (zip (0..(length state.annotations)) state.annotations)

canvasID :: String
canvasID = "canvas"

-- TODO Make this adaptable/adaptive
canvasWidth :: Int
canvasWidth = 1000

-- TODO Make this adaptable/adaptive
canvasHeight :: Int
canvasHeight = 100

canvas' = canvas canvasWidth canvasHeight

canvas w h = HH.canvas [ HP.id canvasID, HP.width w, HP.height h ]

handleAction' :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction' action = do
  handleAction action
  state <- H.get
  H.liftEffect $ void $ unsafePartial do
    Just canvas <- getCanvasElementById canvasID
    ctx <- getContext2D canvas
    setFillStyle ctx "#00F"
    strokePath ctx $ rect ctx
      { x: 0.0
      , y: 0.0
      , width:  I.toNumber canvasWidth
      , height: I.toNumber canvasHeight
      }
    C.setStrokeStyle ctx "#00F"
    drawNumberLine ctx state


handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
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

{-
i is index, a is annotation.

-}
mkAnnotationInput :: forall m. Int -> Annotation -> HH.ComponentHTML Action () m
mkAnnotationInput i a =
  HH.div_
    [ HH.text (show $ i + 1)
    , HH.input
      [ HP.value (show a.place)
      , HE.onValueChange \p ->
         Edit i a { place = fromMaybe a.place $ N.fromString p }
      ]
    , HH.input
      [ HP.value a.label
      , HE.onValueChange \l ->
         Edit i a { label = l }
      ]
    , HH.button [ HE.onClick \_ -> Remove i ] [ HH.text "-" ]
    ]

mkSettingsInput :: forall m. Number -> (Number -> Action) -> HH.ComponentHTML Action () m
mkSettingsInput oldVal action =
  HH.input
  [ HP.value (show oldVal)
  , HE.onValueChange \s -> action (fromMaybe oldVal $ N.fromString s :: _)
  ]

{-
Based on https://gist.github.com/jwir3/d797037d2e1bf78a9b04838d73436197 .
-}
drawArrowTriangle ctx fromX fromY toX toY radius = do

  -- Arrow line
  C.moveTo ctx fromX fromY
  C.lineTo ctx toX toY
  C.stroke ctx

  -- var x_center = to.x;
  -- var y_center = to.y;
  let centerX = toX
  let centerY = toY

  -- var angle;
  -- var x;
  -- var y;

  -- context.beginPath();
  C.beginPath ctx

  -- angle = Math.atan2(to.y - from.y, to.x - from.x)
  -- x = radius * Math.cos(angle) + x_center;
  -- y = radius * Math.sin(angle) + y_center;
  let angle = M.atan2 (toY - fromY) (toX - fromX)
  let x = radius * M.cos angle + centerX
  let y = radius * M.sin angle + centerY

  -- context.moveTo(x, y);
  C.moveTo ctx x y

  -- angle += (1.0/3.0) * (2 * Math.PI)
  -- x = radius * Math.cos(angle) + x_center;
  -- y = radius * Math.sin(angle) + y_center;
  let angle2 = angle + (1.0 / 3.0) * (2.0 * M.pi)
  let x = radius * M.cos angle2 + centerX
  let y = radius * M.sin angle2 + centerY

  -- context.lineTo(x, y);
  C.lineTo ctx x y

  -- angle += (1.0/3.0) * (2 * Math.PI)
  -- x = radius *Math.cos(angle) + x_center;
  -- y = radius *Math.sin(angle) + y_center;
  let angle3 = angle2 + (1.0 / 3.0) * (2.0 * M.pi)
  let x = radius * M.cos angle3 + centerX
  let y = radius * M.sin angle3 + centerY

  -- context.lineTo(x, y);
  C.lineTo ctx x y

  -- context.closePath();
  C.closePath ctx

  -- context.fill();
  C.fill ctx

drawArrow ctx fromX fromY toX toY headLength = do
  let angle = M.atan2 (toY - fromY) (toX - fromX)

  C.beginPath ctx

  C.moveTo ctx fromX fromY
  C.lineTo ctx toX toY
  C.moveTo ctx (toX - headLength * M.cos (angle - M.pi / 6.0))
    (toY - headLength * M.sin (angle - M.pi / 6.0))
  C.lineTo ctx toX toY
  C.lineTo ctx (toX - headLength * M.cos (angle + M.pi / 6.0))
    (toY - headLength * M.sin (angle + M.pi / 6.0))

  C.stroke ctx


drawNumberLine ctx numberLine = do
  let fromX = 0.0 + 10.0
  let toX = I.toNumber canvasWidth - 10.0
  let fromY = I.toNumber canvasHeight / 2.0
  let toY = fromY
  let headLength = 20.0

  drawArrow ctx fromX fromY toX toY headLength

  let startC = fromX + 10.0
  let endC = toX - 2.0 * headLength

  let start = numberLine.start
  let end = numberLine.end
  let step = numberLine.step

  -- This is one tick short …
  let nSteps = I.floor $ (end - start) / step
  -- … but we start counting from 0 which adds one.
  let steps = (map (\n -> I.toNumber n * step + start) $ 0..nSteps) `snoc` end
  let stepsCoords = map (toCoord start end startC endC) steps

  let tickLength = 10.0
  sequence_ $ map (drawTick ctx fromY tickLength) stepsCoords
  C.stroke ctx

drawTick ctx y len coord = do
  C.moveTo ctx coord (y - len)
  C.lineTo ctx coord (y + len)
  C.stroke ctx

toCoord startN endN startC endC num =
  startC + (num / (endN - startN)) * (endC - startC)
