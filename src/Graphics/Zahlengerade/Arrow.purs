module Graphics.Zahlengerade.Arrow where

import Prelude

import Data.Int as I
import Effect (Effect)
import Math as M
import Graphics.Canvas as C
import Graphics.Zahlengerade.Canvas (Canvas, newLineWidth)

type Coord =
  { x :: Number
  , y :: Number
  }

type Arrow =
  { from :: Coord
  , to :: Coord
  , headLength :: Number
  }

arrow :: Canvas -> Number -> Number -> Arrow
arrow cv headLength xOffset =
  let y = I.toNumber cv.height / 2.0
  in
    { from :
      { x : xOffset
      , y : y
      }
    , to :
      { x : I.toNumber cv.width - xOffset
      , y : y
      }
    , headLength : headLength
    }

-- | Only adds to the current path, does neither call
-- | 'Graphics.Canvas.beginPath' nor 'Graphics.Canvas.stroke'.
drawArrow
  :: C.Context2D
  -> Arrow
  -> Effect Unit
drawArrow ctx arr = do
  let to = arr.to
  let from = arr.from
  -- TODO Make adaptable
  newLineWidth ctx 2.0

  let angle = M.atan2 (to.y - from.y) (to.x - from.x)

  C.moveTo ctx from.x from.y
  C.lineTo ctx to.x to.y
  C.moveTo ctx (to.x - arr.headLength * M.cos (angle - M.pi / 6.0))
    (to.y - arr.headLength * M.sin (angle - M.pi / 6.0))
  C.lineTo ctx to.x to.y
  C.lineTo ctx (to.x - arr.headLength * M.cos (angle + M.pi / 6.0))
    (to.y - arr.headLength * M.sin (angle + M.pi / 6.0))
