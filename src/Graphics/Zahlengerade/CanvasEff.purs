module Graphics.Zahlengerade.CanvasEff
( module Graphics.Zahlengerade.CanvasEff
, module X
)
where
-- TODO Rename this module

import Prelude

import Effect.Class (liftEffect) as X
import Control.Monad.Reader (class MonadAsk, ask) as X

import Control.Monad.Reader.Trans (ReaderT, ask, runReaderT)
import Effect (Effect)
import Effect.Class (liftEffect)
import Graphics.Canvas as C

-- | Canvas effects.
type CanvasEff = ReaderT Context Effect

runCanvasEff :: forall a. CanvasEff a -> Context -> Effect a
runCanvasEff = runReaderT

type Context =
  { ctx :: C.Context2D
  , res :: Number
  }

newLineWidth :: Number -> CanvasEff Unit
newLineWidth width = do
  { ctx } <- ask
  -- I need to stroke and begin a new path whenever I change the line width.
  liftEffect $ do
    C.stroke ctx
    C.beginPath ctx
    C.setLineWidth ctx width
