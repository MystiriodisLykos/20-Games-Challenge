{-# LANGUAGE Arrows #-}

module Ball ( BallState(..), BallInput(..), Bounce
            , vertical, horizontal
            , ball, drawBall
            ) where

import Control.Arrow                      ( returnA )
import FRP.Yampa                          ( Event, SF, VectorSpace((*^), dot, norm)
                                          , accumHold, integral, notYet
                                          , switch, tag)
import Graphics.Gloss                     ( Color
                                          , Picture (Color, Translate)
                                          , circleSolid)
import Linear.V2   ( V2 (V2) )

import Linear.VectorSpace()

-- TODO: add ball radius
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
ball' initial = switch ball'' ball'
  where
    ball'' = proc bi -> do
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
