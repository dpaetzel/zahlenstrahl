module Graphics.Zahlengerade.Arrow where

import Prelude

import Data.Int as I
import Data.Vector.Polymorphic.Types (Vector2(..))
import Math as M
import Graphics.CanvasAction.Path as CA

import Graphics.Zahlengerade.Canvas (Canvas)

type Coord =
  { x :: Number
  , y :: Number
  }

type Arrow =
  { from :: Coord
  , to :: Coord
  , headLength :: Number
  }

arrow :: Canvas -> Number -> Number -> Number -> Arrow
arrow cv headLength xOffsetL xOffsetR =
  let y = I.toNumber cv.height / 2.0
  in
    { from :
      { x : xOffsetL
      , y : y
      }
    , to :
      { x : I.toNumber cv.width - xOffsetR
      , y : y
      }
    , headLength : headLength
    }

-- | Only adds to the current path, does neither call
-- | 'Graphics.Canvas.beginPath' nor 'Graphics.Canvas.stroke'.
drawArrow
  :: Arrow
  -> CA.PathAction Unit
drawArrow arr = do
  let to = arr.to
  let from = arr.from

  let angle = M.atan2 (to.y - from.y) (to.x - from.x)

  CA.moveTo $ Vector2 from.x from.y
  CA.lineTo $ Vector2 to.x to.y
  CA.moveTo $
    Vector2
      (to.x - arr.headLength * M.cos (angle - M.pi / 6.0))
      (to.y - arr.headLength * M.sin (angle - M.pi / 6.0))
  CA.lineTo $ Vector2 to.x to.y
  CA.lineTo $
    Vector2
      (to.x - arr.headLength * M.cos (angle + M.pi / 6.0))
      (to.y - arr.headLength * M.sin (angle + M.pi / 6.0))
