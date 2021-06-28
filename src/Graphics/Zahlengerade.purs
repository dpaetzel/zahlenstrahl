module Graphics.Zahlengerade
( module Graphics.Zahlengerade.Canvas
, module Graphics.Zahlengerade
)
where

import Prelude

import Control.Monad.Reader (asks)
import Data.Array (zipWith, (..), (\\))
import Data.Int as I
import Data.Traversable (sequence_)
import Data.String as S
import Graphics.Canvas as C
import Math as M

import Graphics.Zahlengerade.Arrow (Arrow, arrow, drawArrow)
import Graphics.Zahlengerade.Canvas
  ( Canvas
  , asRect
  , clearCanvas
  )
import Graphics.Zahlengerade.CanvasEff

-- TODO Make adaptable
fontHeight :: Int
fontHeight = 20

-- TODO Make adaptable
tickLength :: Number
tickLength = 16.0

-- TODO Make adaptable
mediumTickLength :: Number
mediumTickLength = 10.0

-- TODO Make adaptable
miniTickLength :: Number
miniTickLength = 6.0

-- TODO Make adaptable
markerLength :: Number
markerLength = 50.0

type Annotation = { place :: Number, label :: String }

-- | Default annotation.
defAnnotation :: Annotation
defAnnotation = { place : 0.0, label : "0" }

type NumberLine =
    { start       :: Number
    , end         :: Number
    , step        :: Number
    , mediumStep  :: Number
    , miniStep    :: Number
    , annotations :: Array Annotation
    , canvas :: Canvas
    , resolution :: Int
    }

-- | Default canvas dimensions.
defCanvas :: Canvas
defCanvas = { width : 1000, height : 200 }

-- | Default number line configuration.
defNumberLine :: NumberLine
defNumberLine =
  { start : 0.0
  , end : 10.0
  , step : 1.0
  , mediumStep : 0.5
  , miniStep : 0.1
  , annotations :
    [ { place : 3.14, label : "π"}
    , { place : 2.71, label : "e"}
    ]
  , canvas : defCanvas
  , resolution : 1
  }

-- | Represents a transformation from one (one-dimensional) space to another.
-- |
-- | Used to convert between numbers entered by the user and x positions on the
-- | canvas.
type Transformation =
  { from :: { start :: Number, end :: Number }
  , to :: { start :: Number, end :: Number}
  }

-- | Retrieves the transformation described implicitly by the given number line
-- | and arrow configurations.
transformation :: NumberLine -> Arrow -> Transformation
transformation numLine arr =
  { from : { start : numLine.start, end : numLine.end }
  , to :
    { start : arr.from.x + 0.6 * arr.headLength
    , end : arr.to.x - 1.2 * arr.headLength
    }
  }

-- | Applies a transformation to a number (i.e. transforming it to the
-- | corresponding number in the target space).
transform :: Transformation -> Number -> Number
transform t n =
  t.to.start
    + ((n - t.from.start) / (t.from.end - t.from.start))
      * (t.to.end - t.to.start)
  -- TODO Probably round x values here to the nearest .5? Or, depending on
  -- linewidth of the tick to .5 or .0

-- | Rounds to the closest number of the form *.5.
-- |
-- | Useful since lines in HTML canvas can be blurry if not drawn at
-- | .5-positions.
roundToDot5 :: Number -> Number
roundToDot5 = (_ + 0.5) <<< M.round

-- | Given a start and end and a step size, provides the set (array) of steps.
steps
  :: forall r
  . { start :: Number, end :: Number | r }
  -> Number
  -> Array Number
steps { start, end } d =
  let
    n = I.floor $ (end - start) / d
    step k = I.toNumber k * d + start
  in
    map step $ 0..n

-- | Divides the transformation's entire domain into equidistant steps of the
-- | provided length and then transforms those steps to the transformation's
-- | target space.
xCoordsSteps :: Transformation -> Number -> Array Number
xCoordsSteps t = map (roundToDot5 <<< transform t) <<< steps t.from

-- | Transforms a number to a (German) number label.
toLabel :: Number -> String
toLabel num =
  S.replace (S.Pattern ".") (S.Replacement ",") $
  if num - M.floor num == 0.0 then show (I.floor num) else show num

-- | Draws the number line described by the provided config to the provided
-- | context's canvas.
-- |
-- | Only adds to the current path, does neither call
-- | 'Graphics.Canvas.beginPath' nor 'Graphics.Canvas.stroke'.
-- drawNumberLine :: forall m. MonadCanvas m => NumberLine -> m Unit
drawNumberLine :: NumberLine -> CanvasEff Unit
drawNumberLine numLine = do

  let y = roundToDot5 $ I.toNumber numLine.canvas.height / 2.0

  -- Make sure that the first label is not cut off.
  xOffset <- (_ / 2.0) <$> (labelWidth $ toLabel numLine.start)
  let arr = arrow numLine.canvas tickLength xOffset
  drawArrow arr

  let t = transformation numLine arr

  let xs = xCoordsSteps t numLine.step
  drawTicks y tickLength xs

  let labelSteps = steps t.from numLine.step
  let labels = map toLabel labelSteps
  drawTickLabels y tickLength labels xs

  let xsMed = (_ \\ xs) $ xCoordsSteps t numLine.mediumStep
  drawTicks y mediumTickLength xsMed

  let xsMini = (_ \\ (xs <> xsMed)) $ xCoordsSteps t numLine.miniStep
  drawTicks y miniTickLength xsMini

  let xsAnn = map (roundToDot5 <<< transform t <<< _.place) numLine.annotations
  drawAnnotations y markerLength (map _.label numLine.annotations) xsAnn

-- | Given a number line's y-coordinate and a set of x-coordinates, draws a tick
-- | of the given length for each of the x-coordinates to the provided context's
-- | canvas at that position.
-- |
-- | Only adds to the current path, does neither call
-- | 'Graphics.Canvas.beginPath' nor 'Graphics.Canvas.stroke'.
drawTicks
  :: Number
  -> Number
  -> Array Number
  -> CanvasEff Unit
drawTicks y len xs = sequence_ $ map (drawTick y len) xs

-- | Given a number line's y-coordinate as well as an x-coordinate and a length,
-- | draws a tick to the provided context's canvas at that position.
-- |
-- | Only adds to the current path, does neither call
-- | 'Graphics.Canvas.beginPath' nor 'Graphics.Canvas.stroke'.
drawTick
  :: Number -> Number -> Number -> CanvasEff Unit
drawTick y len x = do
  -- TODO Make adaptable
  newLineWidth 1.0

  { ctx } <- ask
  liftEffect $ do
    C.moveTo ctx x (y - len)
    C.lineTo ctx x (y + len)

-- | Given a number line's y-coordinate and sets of x-coordinates and labels,
-- | draws the tick scale labels to the provided context's canvas at that
-- | position. Tick scale labels are, by default, drawn below the number line.
-- |
-- | Only adds to the current path, does neither call
-- | 'Graphics.Canvas.beginPath' nor 'Graphics.Canvas.stroke'.
drawTickLabels
  :: Number
  -> Number
  -> Array String
  -> Array Number
  -> CanvasEff Unit
drawTickLabels y tickLen labels xs =
  sequence_ $ zipWith (drawLabel y tickLen) labels xs

-- | Given a number line's y-coordinate and sets of x-coordinates and
-- | annotations, draws the labels to the provided context's canvas at that
-- | position. Annotations are drawn above the number line with markers which
-- | are basically (usually longer) ticks that do not cross the line.
-- |
-- | Only adds to the current path, does neither call
-- | 'Graphics.Canvas.beginPath' nor 'Graphics.Canvas.stroke'.
drawAnnotations
  :: Number
  -> Number
  -> Array String
  -> Array Number
  -> CanvasEff Unit
drawAnnotations y markerLen anns xs =
  sequence_ $ zipWith (drawAnnotation y markerLen) anns xs

-- | Draw a single annotation, see `drawAnnotations`.
-- |
-- | Only adds to the current path, does neither call
-- | 'Graphics.Canvas.beginPath' nor 'Graphics.Canvas.stroke'.
drawAnnotation
  :: Number
  -> Number
  -> String
  -> Number
  -> CanvasEff Unit
drawAnnotation y markerLen ann x = do
  -- TODO Make adaptable
  newLineWidth 1.0
  { ctx } <- ask
  liftEffect $ do
    C.moveTo ctx x y
    C.lineTo ctx x (y - markerLen)
  drawLabel y (- markerLen) ann x

-- | Given a number line's y-coordinate, a distance (in x), a label text and an
-- | x-coordinate, draws a labels with the corresponding text to the provided
-- | context's canvas at that position.
-- |
-- | Only adds to the current path, does neither call
-- | 'Graphics.Canvas.beginPath' nor 'Graphics.Canvas.stroke'.
drawLabel
  :: Number
  -> Number
  -> String
  -> Number
  -> CanvasEff Unit
drawLabel y dist label x = do
  width <- labelWidth label
  setFont
  { ctx } <- ask
  fh <- fontHeight
  liftEffect $ C.fillText ctx label (x - width / 2.0) (y + dist +
    if dist > 0.0 then I.toNumber fh else - I.toNumber fh / 4.0)

setFont :: CanvasEff Unit
setFont = do
  { ctx } <- ask
  fh <- fontHeight
  liftEffect $ C.setFont ctx (show fh <> "px Arial")

labelWidth :: String -> CanvasEff Number
labelWidth label = do
  setFont
  { ctx } <- ask
  { width } <- liftEffect $ C.measureText ctx label
  pure width
