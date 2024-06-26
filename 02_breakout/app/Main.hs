{-# LANGUAGE Arrows #-}

import Control.Arrow                      ( Arrow, returnA, (>>>), (^>>), (>>^), (&&&), arr )
import Control.Applicative                ( liftA2 )
import FRP.Yampa                          ( SF, Event (Event, NoEvent), VectorSpace((*^), dot, norm)
                                          , tag, catEvents, accumHold, constant
                                          , accumHoldBy, edgeTag, repeatedly
                                          , edge, iPre, merge, integral)
import Graphics.Gloss                     ( Display (InWindow)
                                          , Picture (Pictures, Translate)
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

import Ball (BallState(..), BallInput(..), Bounce, vertical, horizontal, drawBall, ball')
import Paddle (PaddleDirection(..), PaddleInput(..), PaddleState(..), paddle, drawPaddle)
import Linear.GJK (minkCircle, minkRectangle)

data GameInput = GameInput {
  keyLeft :: G.KeyState,
  keyRight :: G.KeyState,
  screenSize :: V2 Int
} deriving Show

type Score = Either () ()

paddleD :: G.KeyState -> G.KeyState -> PaddleDirection
paddleD G.Down G.Up = PaddleLeft
paddleD G.Down G.Down = PaddleStop
paddleD G.Up G.Down = PaddleRight
paddleD _ _ = PaddleStop

merge' :: [Event (a -> a)] -> Event (a -> a)
merge' = (fmap $ foldl1 (.)) . catEvents

paddleInput :: SF (GameInput, PaddleState) PaddleInput
paddleInput = proc (gi, ps) -> do
  -- TODO: stop paddle when it reaches the edge of screen
  returnA -< PaddleInput $ paddleD (keyLeft gi) (keyRight gi)

wallCollision :: SF (GameInput, BallState) (Event Bounce)
wallCollision = proc (gi, bs) -> do
  let
    (V2 w h) = screenSize gi
    (V2 x y) = bP bs
  t <- edgeTag vertical -< y + 10 >= (int h)/2
  l <- edgeTag horizontal -< x + 10 >= (int w)/2
  r <- edgeTag horizontal -< (-x) + 10 >= (int w)/2
  returnA -< merge' [t, l, r]
  where
    int = fromIntegral

paddleCollision :: SF (PaddleState, BallState) (Event Bounce)
paddleCollision = proc (ps, bs) -> do
  let
    p = minkRectangle (pP ps) (pS ps)
    b = minkCircle 10 (V2 0 0)
  c <- edgeTag vertical -< fromMaybe False (collision 10 p b) && (bP bs) `over` (pP ps)
  returnA -< c
  where
    over (V2 _ a) (V2 _ b) = a <= b

ballCollision :: SF (GameInput, PaddleState, BallState) (Event Bounce)
ballCollision = proc (gi, p, b) -> do
  wc <- wallCollision -< (gi, b)
  pc <- paddleCollision -< (p, b)
  returnA -< merge' [wc, pc]

reset :: SF (GameInput, BallState) (Event ())
reset = proc (gi, bs) -> do
  let
    (V2 _ h) = screenSize gi
    (V2 _ y) = bP bs
  r <- edge -< (-y) > (fromIntegral h)/2
  returnA -< r

paddle' :: SF GameInput PaddleState
paddle' = paddle (PaddleState (V2 0 (-200)) 200 (V2 60 5) black) paddleInput

type Pos = V2 Double
type Vel = V2 Double

type BounceV = Vel -> Vel
type BounceE = Event BounceV

-- Doesn't always work as expected when the surface
-- vector is parallel or orthogonal to the vector
-- in that case bounce twice
-- TODO: can I change this function to do 2 bounces?
bounce :: V2 Double -> BounceV
bounce s a = (norm a / norm b) *^ b
  where
    b = a - (2 * (dot a s) *^ s)

vertical' :: BounceV
vertical' = bounce $ V2 0 1

horizontal' :: BounceV
horizontal' = bounce $ V2 1 0

type CircleMink = Mink (Double, Pos)
type BallMink = CircleMink
type RectangleMink = Mink [V2 Double]
type PaddleMink = RectangleMink
type BrickMink = RectangleMink

position :: Pos -> SF Vel Pos
position p0 = integral >>^ ((+) p0)

bVelocity :: Vel -> SF BounceE Vel
bVelocity v0 = accumHold v0

collisionCircle :: Double -> SF Pos BallMink
collisionCircle r = arr $ minkCircle r

-- flip size and position and switch to operating on doubles
minkRectangle' :: V2 Double -> V2 Double -> Mink [V2 Double]
minkRectangle' s p = minkRectangle (double2Float <$> s) (double2Float <$> p)

collisionRectangle :: V2 Double -> SF Pos (Mink [V2 Double])
collisionRectangle s = arr $ minkRectangle' s

wallBounce :: SF (BallMink, RectangleMink) BounceE
wallBounce = repeatedly 3 horizontal'

brickBounce :: SF (BallMink, [BrickMink]) (BounceE, Event Int)
brickBounce = repeatedly 20 horizontal' &&& repeatedly 20 0

paddleBounce :: SF (BallMink, PaddleMink) BounceE
paddleBounce = repeatedly 2 vertical'

-- (>+>) :: Arrow a => a b c -> a c d -> a b (c, d)
-- (>+>) a b = a >>> ((arr id) &&& b)
-- infixr 4 >+>

-- goal
-- ball = (wallCollision &+& bricksCollision &+& paddleCollision) >>> velocity (V2 200 400) >>> position (V2 0 0)
-- I like this particular formulation for the ball, though I do need to add in a reset
-- It still requires that something else figures out what the ball actually bounces on, good
-- And returns all the needed information for other things to react to it.
-- Though the returning all the information that other things need to react to in this instance is kinda
-- hapenstance.
ballTest :: SF BounceE BallMink
ballTest = (bVelocity $ V2 50 50) >>> (position $ V2 0 0) >>> (collisionCircle 10)

-- brickTest :: SF () BrickMink
-- brickTest = (constant $ V2 0 0) >>> (position $ V2 20 20) >>> (collisionRectangle (V2 20 10))

paddleTest :: SF Vel PaddleMink
paddleTest = (position $ V2 (-100) 0) >>> (collisionRectangle $ V2 20 1)

ball = ball' (BallState (V2 0 0) (V2 200 400) black)

parseGameInput :: GameInput -> InputEvent -> GameInput
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyLeft) G.Down _ _)   = gi { keyLeft = G.Down }
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyLeft) G.Up _ _)     = gi { keyLeft = G.Up }
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyRight) G.Down _ _) = gi { keyRight = G.Down }
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyRight) G.Up _ _)   = gi { keyRight = G.Up }
parseGameInput gi (G.EventResize (x, y)) = gi {screenSize = V2 x y}
parseGameInput gi _ = gi

input :: SF (Event InputEvent) GameInput
input = accumHoldBy parseGameInput $ GameInput G.Up G.Up (V2 100 100)

game :: SF GameInput Picture
game = proc gi -> do
  p <- paddle' -< gi
  rec
    r <- reset -< (gi, b)
    r' <- iPre NoEvent -< r `tag` ()
    -- TODO: why does `score` need to be delayed with `pre`
    c <- ballCollision -< (gi, p, b)
    b <- ball -< BallInput c r'
  returnA -< Pictures [(drawBall b), (drawPaddle p)]

-- I like this formulation where each bounce type is its own signal funciton
-- Even though its a little harder to add a new thing to bounce off of it
-- does provide more flexibility on how things bounce and that each type of
-- bounce can bounce in a different way.
game' :: SF GameInput Picture
game' = proc _ -> do
  rec
    wb <- wallBounce -< (b, 1)
    pb <- paddleBounce -< (b, p)
    p <- paddleTest -< V2 0 0
    b@((_, bp), _) <- ballTest -< merge' [wb, pb]
  returnA -< drawBall $ BallState (double2Float <$> bp) (V2 0 0) black

defaultPlay :: SF (Event InputEvent) Picture -> IO ()
defaultPlay = playYampa (InWindow "Pong" (300, 500) (200, 200)) white 60

main :: IO ()
main = defaultPlay $ input >>> game'

