module Graphics.Zahlengerade
( module Graphics.Zahlengerade.Canvas
, module Graphics.Zahlengerade
)
where

import Prelude

import Data.Array (zipWith, (..), (\\))
import Data.Foldable (foldM, maximum)
import Data.Int as I
import Data.Maybe (fromMaybe)
import Data.Traversable (sequence_)
import Data.String as S
import Graphics.CanvasAction
  ( class MonadCanvasAction
  , fillText
  , measureText
  , setFont
  , setLineWidth
  ) as CA
import Graphics.CanvasAction.Path
  ( PathAction
  , lineTo
  , moveTo
  , runPath
  , stroke
  ) as CA
import Data.Vector.Polymorphic.Types (Vector2(..))
import Math as M

import Graphics.Zahlengerade.Arrow (Arrow, arrow, drawArrow)
import Graphics.Zahlengerade.Canvas
  ( Canvas
  , asRect
  , clearCanvas
  )

-- TODO Make adaptable
fontHeight :: Int
fontHeight = 20

-- TODO Make adaptable
markerLength :: Number
markerLength = 50.0

type Step =
  { name :: String
  , width :: Number
  , tickLength :: Number
  , labelled :: Boolean
  }

type Annotation = { place :: Number, label :: String }

-- | Default annotation.
defAnnotation :: Annotation
defAnnotation = { place : 0.0, label : "0" }

type NumberLine =
    { start       :: Number
    , end         :: Number
    , steps       :: Array Step
    , annotations :: Array Annotation
    , canvas      :: Canvas
    }

-- | Default canvas dimensions.
defCanvas :: Canvas
defCanvas = { width : 1000, height : 200 }

-- | Default number line configuration.
defNumberLine :: NumberLine
defNumberLine =
  { start : 0.0
  , end : 10.0
  , steps :
    [ { name : "Schritt", width : 1.0, tickLength : 16.0, labelled : true }
    , { name : "Mittelschritt", width : 0.5, tickLength : 10.0, labelled : false }
    , { name : "Minischritt", width : 0.1, tickLength : 6.0, labelled : false }
    ]
  , annotations :
    [ { place : 3.14, label : "π"}
    , { place : 2.71, label : "e"}
    ]
  , canvas : defCanvas
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
    n = nSteps { start, end } d
    step k = I.toNumber k * d + start
  in
    map step $ 0..n

-- | Given a start and end and a step size, provides the number of steps.
nSteps
  :: forall r
  . { start :: Number, end :: Number | r }
  -> Number
  -> Int
nSteps { start, end } d = I.floor $ (end - start) / d

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
drawNumberLine :: forall m. CA.MonadCanvasAction m => NumberLine -> m Unit
drawNumberLine numLine = do

  let y = roundToDot5 $ I.toNumber numLine.canvas.height / 2.0

  -- Make sure that neither the first or last label is cut off.
  xOffsetL <- (_ / 2.0) <$> (labelWidth $ toLabel numLine.start)
  xOffsetR <- (_ / 2.0) <$> (labelWidth $ toLabel numLine.end)

  let maxTickLength = fromMaybe 16.0 $ maximum $ map _.tickLength numLine.steps
  let arr = arrow numLine.canvas maxTickLength xOffsetL xOffsetR

  CA.setLineWidth 2.0
  flip bind CA.stroke $ CA.runPath $ drawArrow arr
  CA.setLineWidth 1.0

  let t = transformation numLine arr

  _ <- foldM (drawTicks y t) [] numLine.steps

  drawAnnotations y t markerLength numLine.annotations

-- | Given a number line's y-coordinate and a set of x-coordinates, draws a tick
-- | of the given length for each of the x-coordinates to the provided context's
-- | canvas at that position. If the ticks are to be labelled, label them.
-- |
-- | Since 'drawTickLabels' is a 'MonadCanvasAction' (and not a 'PathAction'),
-- | this is, too.
drawTicks
  :: forall m. CA.MonadCanvasAction m
  => Number
  -> Transformation
  -> Array Number
  -> Step
  -> m (Array Number)
drawTicks y t dontDraw step = do
  let xs = xCoordsSteps t step.width \\ dontDraw

  flip bind CA.stroke $ CA.runPath $ do
    sequence_ $ map (drawTick y step.tickLength) xs

  when step.labelled $ do
    let labelSteps = steps t.from step.width
    let labels = map toLabel labelSteps
    drawTickLabels y step.tickLength labels xs

  pure $ xs <> dontDraw


-- | Given a number line's y-coordinate as well as an x-coordinate and a length,
-- | constructs a path action that draws a tick to the provided context's canvas
-- | at that position.
-- |
-- | Only adds to the current path, does neither call
-- | 'Graphics.Canvas.beginPath' nor 'Graphics.Canvas.stroke'.
drawTick
  :: Number -> Number -> Number -> CA.PathAction Unit
drawTick y len x = do
  CA.moveTo $ Vector2 x (y - len)
  CA.lineTo $ Vector2 x (y + len)

-- | Given a number line's y-coordinate and sets of x-coordinates and labels,
-- | draws the tick scale labels to the provided context's canvas at that
-- | position. Tick scale labels are, by default, drawn below the number line.
drawTickLabels
  :: forall m. CA.MonadCanvasAction m
  => Number
  -> Number
  -> Array String
  -> Array Number
  -> m Unit
drawTickLabels y tickLen labels xs =
  sequence_ $ zipWith (drawLabel y tickLen) labels xs

-- | Given a number line's y-coordinate and sets of x-coordinates and
-- | annotations, draws the labels to the provided context's canvas at that
-- | position. Annotations are drawn above the number line with markers which
-- | are basically (usually longer) ticks that do not cross the line.
drawAnnotations
  :: forall m. CA.MonadCanvasAction m
  => Number
  -> Transformation
  -> Number
  -> Array Annotation
  -> m Unit
drawAnnotations y t markerLen anns = do
  let xs = map (roundToDot5 <<< transform t <<< _.place) anns
  let labels = map _.label anns
  sequence_ $ zipWith (drawLabel y (- markerLength)) labels xs
  flip bind CA.stroke $ CA.runPath $ do
    sequence_ $ zipWith (drawAnnotationMarker y markerLength) labels xs

-- | Constructs a path action that draws a single annotation marker.
-- | Annotation markers are basically (usually longer) ticks that do not cross
-- | the number line.
drawAnnotationMarker
  :: Number
  -> Number
  -> String
  -> Number
  -> CA.PathAction Unit
drawAnnotationMarker y markerLen ann x = do
  CA.moveTo $ Vector2 x y
  CA.lineTo $ Vector2 x (y - markerLen)

-- | Given a number line's y-coordinate, a distance (in x), a label text and an
-- | x-coordinate, draws a labels with the corresponding text to the provided
-- | context's canvas at that position.
-- |
-- | Since 'fillText' is a 'MonadCanvasAction' (and not a 'PathAction'), this
-- | is, too.
drawLabel
  :: forall m. CA.MonadCanvasAction m
  => Number
  -> Number
  -> String
  -> Number
  -> m Unit
drawLabel y dist label x = do
  width <- labelWidth label
  setFont
  let fh = fontHeight
  CA.fillText label $ Vector2 (x - width / 2.0) (y + dist +
    if dist > 0.0 then I.toNumber fh else - I.toNumber fh / 4.0)

setFont :: forall m. CA.MonadCanvasAction m => m Unit
setFont = do
  CA.setFont (show fontHeight <> "px Arial")

labelWidth :: forall m. CA.MonadCanvasAction m => String -> m Number
labelWidth label = do
  setFont
  { width } <- CA.measureText label
  pure width
