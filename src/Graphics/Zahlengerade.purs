module Graphics.Zahlengerade
( module Graphics.Zahlengerade.Canvas
, module Graphics.Zahlengerade
)
where

import Prelude

import Data.Array (deleteAt, length, snoc, updateAt, zip, zipWith, (..))
import Data.Int as I
import Data.Traversable (sequence_)
import Effect (Effect)
import Math as M
import Graphics.Canvas as C
import Graphics.Zahlengerade.Canvas

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

defNumberLine :: NumberLine
defNumberLine =
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

-- | Only adds to the current path, does neither call
-- | 'Graphics.Canvas.beginPath' nor 'Graphics.Canvas.stroke'.
drawNumberLine ctx cv numberLine = do
  let y = I.toNumber cv.height / 2.0
  let arrow = {
    from : {
        x : 0.0 + 10.0,
        y : y
    },
    to : {
        x : I.toNumber cv.width - 10.0,
        y : y
    }
  }

  -- TODO Make adaptive
  let headLength = 15.0
  -- TODO Make adaptive
  -- TODO Move into drawArrow
  let lineWidth = 2.0
  C.setLineWidth ctx lineWidth
  drawArrow ctx arrow.from arrow.to headLength

  let coords = {
    start : arrow.from.x + 10.0,
    end : arrow.to.x - 3.0 * headLength
  }

  let numbers = {
    start : numberLine.start,
    end : numberLine.end,
    step : numberLine.step
  }

  -- TODO Make adaptable
  let tickLength = 15.0
  let mediumTickLength = 10.0
  let miniTickLength = 5.0

  C.setLineWidth ctx lineWidth
  drawTicks ctx false miniTickLength y coords (numbers { step = numberLine.miniStep })
  drawTicks ctx false mediumTickLength y coords (numbers { step = numberLine.mediumStep })
  drawTicks ctx true tickLength y coords (numbers { step = numberLine.step })

  where
    drawTicks ctx labels tickLength y coords numbers = do
      -- This is one tick short …
      let nSteps = I.floor $ (numbers.end - numbers.start) / numbers.step
      -- … but we start counting from 0 which adds one.
      let steps = (map (step numbers) $ 0..nSteps) `snoc` numbers.end
      let stepsCoords = map (toCoord numbers coords) steps

      sequence_ $ map (drawTick ctx y tickLength) stepsCoords

      when labels $ do
        sequence_ <<< zipWith
            (\xCoord num -> drawLabel ctx y tickLength xCoord (show num))
            stepsCoords
          $ steps

    step numbers n = I.toNumber n * numbers.step + numbers.start

    toCoord numbers coords num =
      coords.start
        + ((num - numbers.start) / (numbers.end - numbers.start))
          * (coords.end - coords.start)

-- | Only adds to the current path, does neither call
-- | 'Graphics.Canvas.beginPath' nor 'Graphics.Canvas.stroke'.
drawArrow ctx from to headLength = do
  let angle = M.atan2 (to.y - from.y) (to.x - from.x)

  C.moveTo ctx from.x from.y
  C.lineTo ctx to.x to.y
  C.moveTo ctx (to.x - headLength * M.cos (angle - M.pi / 6.0))
    (to.y - headLength * M.sin (angle - M.pi / 6.0))
  C.lineTo ctx to.x to.y
  C.lineTo ctx (to.x - headLength * M.cos (angle + M.pi / 6.0))
    (to.y - headLength * M.sin (angle + M.pi / 6.0))


-- | Only adds to the current path, does neither call
-- | 'Graphics.Canvas.beginPath' nor 'Graphics.Canvas.stroke'.
drawTick ctx y len x = do
  C.moveTo ctx x (y - len)
  C.lineTo ctx x (y + len)

-- | 'len' is the corresponding tick's length.
-- |
-- | Only adds to the current path, does neither call
-- | 'Graphics.Canvas.beginPath' nor 'Graphics.Canvas.stroke'.
drawLabel ctx y len x text = do
  { width } <- C.measureText ctx text
  let fontHeight = 10.0
  C.fillText ctx text (x - width / 2.0) (y + len + fontHeight)
