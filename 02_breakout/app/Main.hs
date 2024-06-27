{-# LANGUAGE Arrows #-}

import Control.Arrow                      ( Arrow, returnA, (>>>), (^>>), (>>^), (&&&), arr )
import Control.Applicative                ( liftA2 )
import FRP.Yampa                          ( SF, Event (Event, NoEvent), VectorSpace((*^), dot, norm)
                                          , tag, catEvents, accumHold, constant
                                          , accumHoldBy, edgeTag, repeatedly
                                          , edge, iPre, merge, integral
                                          , drSwitch )
import Graphics.Gloss                     ( Display (InWindow)
                                          , Picture (Pictures, Translate, Color)
                                          , Color
                                          , circleSolid, polygon
                                          , white
                                          , black
                                          , text
                                          )
import Graphics.Gloss.Interface.FRP.Yampa ( InputEvent, playYampa )
import Linear.V2 (V2 (V2))
import GJK.Collision (collision)
import GJK.Mink (Mink)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum)
import GHC.Float (double2Float, float2Double)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Debug.Trace (trace)

import Linear.GJK (minkCircle, minkRectangle)
import Linear.VectorSpace ()

-- reset :: SF (GameInput, BallState) (Event ())
-- reset = proc (gi, bs) -> do
--   let
--     (V2 _ h) = screenSize gi
--     (V2 _ y) = bP bs
--   r <- edge -< (-y) > (fromIntegral h)/2
--   returnA -< r

-- paddle' :: SF GameInput PaddleState
-- paddle' = paddle (PaddleState (V2 0 (-200)) 200 (V2 60 5) black) paddleInput

type Pos = V2 Double
type Vel = V2 Double

type BounceV = Vel -> Vel
type BounceE = Event BounceV

type CircleMink = Mink (Double, Pos)
type BallMink = CircleMink
type RectangleMink = Mink [V2 Double]
type PaddleMink = RectangleMink
type BrickMink = RectangleMink

type ScreenSize = V2 Int

data VelDirection = VelForward | VelZero | VelBackward deriving (Show, Eq)

data GameInput = GameInput {
  keyLeft :: G.KeyState,
  keyRight :: G.KeyState,
  screenSize :: V2 Int
} deriving Show

-- Doesn't always work as expected when the surface
-- vector is parallel or orthogonal to the vector
-- in that case bounce twice
-- TODO: can I change this function to do 2 bounces?
bounce :: V2 Double -> BounceV
bounce s a = (norm a / norm b) *^ b
  where
    b = a - (2 * (dot a s) *^ s)

mergeC :: [Event (a -> a)] -> Event (a -> a)
mergeC = (fmap $ foldl1 (.)) . catEvents

vertical :: BounceV
vertical = bounce $ V2 0 1

horizontal :: BounceV
horizontal = bounce $ V2 1 0

position :: Pos -> SF Vel Pos
position p0 = integral >>^ ((+) p0)

bVelocity :: Vel -> SF BounceE Vel
bVelocity v0 = accumHold v0

-- mVelocity :: (Num a) => Vel -> SF a Vel
-- mVelocity v0 = accumHold

lVelocity :: Vel -> SF VelDirection Vel
lVelocity v = arr (\d -> (d' d) * v)
  where
    d' VelForward = 1
    d' VelBackward = -1
    d' _ = 0

collisionCircle :: Double -> SF Pos BallMink
collisionCircle r = arr $ minkCircle r

-- flip size and position and use doubles instead of floats
minkRectangle' :: V2 Double -> V2 Double -> Mink [V2 Double]
minkRectangle' s p = minkRectangle (double2Float <$> p) (double2Float <$> s)

collisionRectangle :: V2 Double -> SF Pos (Mink [V2 Double])
collisionRectangle s = arr $ minkRectangle' s

drawBall :: V2 Float -> Float -> Picture
drawBall (V2 x y) = Translate x y . circleSolid

drawRectangle :: [V2 Float] -> Picture
drawRectangle = polygon . fmap (\(V2 x y) -> (x, y))

wallBounce :: SF (CircleMink, ScreenSize) BounceE
wallBounce = proc (((r, (V2 x y)), _) , (V2 w h)) -> do
  t <- edgeTag vertical -< y + r >= (int h)/2
  l <- edgeTag horizontal -< x + r >= (int w)/2
  r <- edgeTag horizontal -< (-x) + r >= (int w)/2
  returnA -< mergeC [t, l, r]
  where
    int = fromIntegral

brickBounce :: SF (BallMink, [BrickMink]) (BounceE, Event Int)
brickBounce = repeatedly 20 horizontal &&& repeatedly 20 0

paddleBounce :: SF (BallMink, PaddleMink) BounceE
paddleBounce = (fromMaybe False . collision') ^>> edgeTag vertical
  where collision' (a, b) = collision 10 a b

ballReset :: SF (BallMink, ScreenSize) (Event ())
ballReset = proc (((r, (V2 x y)), _) , (V2 w h)) -> edge -< y < -(fromIntegral h)/2

ball :: SF BounceE BallMink
ball = (bVelocity $ V2 50 (-100)) >>> (position $ V2 0 0) >>> (collisionCircle 8)

-- brickTest :: SF () BrickMink
-- brickTest = (constant $ V2 0 0) >>> (position $ V2 20 20) >>> (collisionRectangle (V2 20 10))

paddle :: SF VelDirection PaddleMink
paddle = lVelocity (V2 100 0) >>> (position $ V2 0 (-100)) >>> (collisionRectangle $ V2 50 5)

parseGameInput :: GameInput -> InputEvent -> GameInput
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyLeft) G.Down _ _)   = gi { keyLeft = G.Down }
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyLeft) G.Up _ _)     = gi { keyLeft = G.Up }
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyRight) G.Down _ _) = gi { keyRight = G.Down }
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyRight) G.Up _ _)   = gi { keyRight = G.Up }
parseGameInput gi (G.EventResize (x, y)) = gi {screenSize = V2 x y}
parseGameInput gi _ = gi

input :: SF (Event InputEvent) GameInput
input = accumHoldBy parseGameInput $ GameInput G.Up G.Up (V2 100 100)

paddleD :: G.KeyState -> G.KeyState -> VelDirection
paddleD G.Down G.Up = VelForward
paddleD G.Up G.Down = VelBackward
paddleD _ _ = VelZero

-- game :: SF GameInput Picture
-- game = proc gi -> do
--   p <- paddle' -< gi
--   rec
--     r <- reset -< (gi, b)
--     r' <- iPre NoEvent -< r `tag` ()
--     -- TODO: why does `score` need to be delayed with `pre`
--     c <- ballCollision -< (gi, p, b)
--     b <- ball -< BallInput c r'
--   returnA -< Pictures [(drawBall b), (drawPaddle p)]

-- I like this formulation where each bounce type is its own signal funciton
-- Even though its a little harder to add a new thing to bounce off of it
-- does provide more flexibility on how things bounce and that each type of
-- bounce can bounce in a different way.
game' :: SF GameInput Picture
game' = proc gi -> do
  rec
    wb              <- wallBounce    -< (b, screenSize gi)
    r               <- ballReset     -< (b, screenSize gi)
    pb              <- paddleBounce  -< (b, p)
    p@(ps, _)       <- paddle        -< paddleD (keyRight gi) (keyLeft gi)
    b@((br, bp), _) <- drSwitch ball -< (mergeC [wb, pb], r `tag` ball)
  returnA -< Pictures [ drawBall (double2Float <$> bp) $ double2Float br
                      , drawRectangle ((fmap double2Float) <$> ps)
                      ]

defaultPlay :: SF (Event InputEvent) Picture -> IO ()
defaultPlay = playYampa (InWindow "Pong" (300, 500) (200, 200)) white 60

main :: IO ()
main = defaultPlay $ input >>> game'

