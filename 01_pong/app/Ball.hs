{-# LANGUAGE Arrows #-}

module Ball ( BallState(..)
            , BallInput(..)
            , Bounce
            , vertical
            , horizontal
            , ball
            , drawBall
            ) where

import Control.Concurrent
import Control.Arrow                      ( returnA, (>>>), arr )
import FRP.Yampa                          ( SF, Event (Event, NoEvent)
                                          , VectorSpace(zeroVector, (*^), (^+^), dot, (^/), norm)
                                          , integral, reactimate, now, notYet
                                          , constant, event, tag, hold, after
                                          , mapFilterE, isEvent, edge, lMerge
                                          , identity, accumHold, repeatedly, delayEvent
                                          , dpSwitch, never, edgeTag, mergeBy, switch, parB)
import Graphics.Gloss                     ( Display (InWindow)
                                          , Color
                                          , Picture (Color,Translate, Pictures)
                                          , circleSolid, black, white, red, green, blue, orange, yellow)
import Debug.Trace ( trace )
import Linear.V2   ( V2 (V2), perp, unangle )
import qualified Linear.Vector as Vector
import qualified Linear.Metric as Metric
import Graphics.Gloss.Interface.FRP.Yampa ( InputEvent, playYampa )

import Types (v2x, v2y)

data BallState = BallState {
  bP :: V2 Float,
  bV :: V2 Float,
  bColor :: Color
} deriving (Eq, Show)

data BallInput = BallInput {
  bBounce :: Event Bounce,
  bReset :: Event ()
}

type Bounce = V2 Float -> V2 Float

vertical :: Bounce
vertical v = bounce v (V2 0 1)

horizontal :: Bounce
horizontal v = bounce v (V2 1 0)

-- Doesn't always work as expected when the surface
-- vector is parallel or orthogonal to the vector
-- in that case bounce twice
-- TODO: can I change this function to do 2 bounces?
bounce :: V2 Float -> V2 Float -> V2 Float
bounce a s = (norm a / norm b) *^ b
  where
    b = a - (2 * (dot a s) *^ s)

ball' :: BallState -> SF BallInput BallState
ball' initial = switch ball ball'
  where
    ball = proc bi -> do
      v <- accumHold $ bV initial -< bBounce bi
      p <- integral -< v
      -- Score event needs to be delayed to prevent infinite switching
      s <- notYet -< bReset bi
      let bs = initial {bP = p + bP initial}
      returnA -< (bs, s `tag` initial {bV = v})

ball :: BallState -> SF (a, BallState) BallInput -> SF a BallState
ball initial bi = proc i -> do
  rec
    bi <- bi -< (i, bs)
    bs <- ball' initial -< bi
  returnA -< bs

drawBall :: BallState -> Picture
drawBall (BallState (V2 x y)  _ color) = Color color $ Translate x y $ circleSolid 10

-- Example

exampleBallCollision :: SF BallState (Event Bounce)
exampleBallCollision = proc bs -> do
  vc <- edgeTag vertical -< abs (v2y $ bP bs) >= 200
  hc <- edgeTag horizontal -< abs (v2x $ bP bs) >= 200
  let c = mergeBy (.) vc hc
  returnA -< c

exampleBallScore :: SF a (Event ())
exampleBallScore = repeatedly 20 ()

exampleBallInput :: SF (a, BallState) BallInput
exampleBallInput = proc (_, bs) -> do
  c <- exampleBallCollision -< bs
  s <- exampleBallScore -< bs
  returnA -< BallInput c s

exampleBall :: Color -> V2 Float -> SF a BallState
exampleBall c vi = ball (BallState (V2 0 0) vi c) exampleBallInput

defaultPlay :: SF (Event InputEvent) Picture -> IO ()
defaultPlay = playYampa (InWindow "YampaDemo" (1280, 1050) (200, 200)) white 30

main :: IO ()
main = defaultPlay $ parB ((\b -> b >>> (arr drawBall)) <$> [
  exampleBall black (V2 50 (-50)),
  exampleBall red (V2 50   50 ),
  exampleBall blue (V2 50  100 ),
  exampleBall green (V2 50 (-100)),
  exampleBall orange (V2 0  (-100)),
  exampleBall yellow (V2 50 0 )
  ]) >>> arr Pictures
