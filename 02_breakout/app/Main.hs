{-# LANGUAGE Arrows #-}

import Control.Arrow                      ( returnA, (>>>) )
import FRP.Yampa                          ( SF, Event (Event, NoEvent)
                                          , tag, catEvents
                                          , accumHoldBy, edgeTag
                                          , edge, iPre, merge)
import Graphics.Gloss                     ( Display (InWindow)
                                          , Picture (Pictures, Translate)
                                          , white
                                          , black
                                          , text
                                          )
import Graphics.Gloss.Interface.FRP.Yampa ( InputEvent, playYampa )
import Linear.V2 (V2 (V2))
import GJK.Collision (collision)
import Data.Maybe (fromMaybe)
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
    b = minkCircle 10 (bP bs)
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

drawScore :: (Int, Int) -> Picture
-- drawScore (l, r) | (trace (show l ++ " " ++ show r) False) = undefined
drawScore (l, r) = Pictures [Translate 50 0 $ text (show r), Translate (-100) 0 $ text (show l)]

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

defaultPlay :: SF (Event InputEvent) Picture -> IO ()
defaultPlay = playYampa (InWindow "Pong" (300, 500) (200, 200)) white 60

main :: IO ()
main = defaultPlay $ input >>> game

