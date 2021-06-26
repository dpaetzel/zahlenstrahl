module Graphics.Zahlengerade.Canvas where

import Prelude

import Data.Int as I
import Effect (Effect)
import Graphics.Canvas as C

-- | Helper type for cleaning up code, basically a pair of 'Int's describing
-- | width and height of the canvas.
type Canvas = { width :: Int, height :: Int }

-- | Clears the given canvas in the provided context.
clearCanvas :: C.Context2D -> Canvas -> Effect Unit
clearCanvas ctx cv = do
  C.clearRect ctx (asRect cv)

-- | Transforms a 'Canvas' into a 'Graphics.Canvas.Rectangle'.
asRect :: Canvas -> C.Rectangle
asRect cv =
  { x : 0.0, y:  0.0, width: (I.toNumber cv.width), height: (I.toNumber cv.height)}

-- | Strokes the border of the canvas.
strokeCanvas :: C.Context2D -> Number -> Canvas -> Effect Unit
strokeCanvas ctx lineWidth cv = do
  let cvRect = asRect cv
  let rect =
        { x : cvRect.x + lineWidth
        , y : cvRect.y + lineWidth
        , width : cvRect.width - 2.0 * lineWidth
        , height : cvRect.height - 2.0 * lineWidth
        }
  C.setLineWidth ctx 2.0
  C.rect ctx rect

newLineWidth :: C.Context2D -> Number -> Effect Unit
newLineWidth ctx width = do
  -- I need to stroke and begin a new path whenever I change the line width.
  C.stroke ctx
  C.beginPath ctx
  C.setLineWidth ctx width
