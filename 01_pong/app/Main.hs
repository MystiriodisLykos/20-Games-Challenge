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
  keyUp :: G.KeyState,
  keyDown :: G.KeyState,
  keyW :: G.KeyState,
  keyS :: G.KeyState,
  screenSize :: V2 Int
} deriving Show

-- TODO: must be a better way to handle the input for 2 paddles
paddle1Input :: SF (GameInput, PaddleState) PaddleInput
paddle1Input = proc (gi, ps) -> do
  -- TODO: stop paddle when it reaches the top of the screen
  returnA -< PaddleInput $ d gi
  where
    d GameInput{keyUp = G.Down, keyDown = G.Up}   = PaddleUp
    d GameInput{keyUp = G.Down, keyDown = G.Down} = PaddleStop
    d GameInput{keyUp = G.Up, keyDown = G.Down}   = PaddleDown
    d _ = PaddleStop

paddle2Input :: SF (GameInput, PaddleState) PaddleInput
paddle2Input = proc (gi, ps) -> do
  returnA -< PaddleInput $ d gi
  where
    d GameInput{keyW = G.Down, keyS = G.Up}   = PaddleUp
    d GameInput{keyW = G.Down, keyS = G.Down} = PaddleStop
    d GameInput{keyW = G.Up, keyS = G.Down}   = PaddleDown
    d _ = PaddleStop

wallCollision :: SF (GameInput, BallState) (Event Bounce)
wallCollision = proc (gi, bs) -> do
  let
    (V2 _ h) = screenSize gi
    (V2 _ y) = bP bs
  c <- edgeTag vertical -< abs y + 10 >= (fromIntegral h)/2
  returnA -< c

paddleCollision :: SF (PaddleState, BallState) (Event Bounce)
paddleCollision = proc (ps, bs) -> do
  let
    p = minkRectangle (pP ps) (pS ps)
    b = minkCircle 10 (bP bs)
  c <- edgeTag horizontal -< fromMaybe False (collision 10 p b) && (bP bs) `infront` (pP ps)
  returnA -< c
  where
    infront a b = (abs a) <= (abs b)

ballCollision :: SF (GameInput, PaddleState, PaddleState, BallState, Event a) (Event Bounce)
ballCollision = proc (gi, p1, p2, b, s) -> do
  wc <- wallCollision -< (gi, b)
  pc <- paddleCollision -< (sP (bP b) p1 p2, b)
  returnA -< merge' [wc, pc, s `tag` horizontal]
  where
    merge' = (fmap $ foldl1 (.)) . catEvents
    sP p p1 p2 = if (p >= V2 0 0) then p1 else p2

score :: SF (GameInput, BallState) (Event Score)
score = proc (gi, bs) -> do
  let
    (V2 w _) = screenSize gi
    (V2 x _) = bP bs
  l <- edgeTag $ Left () -< s x w
  r <- edgeTag $ Right ()  -< s (-x) w
  returnA -< merge l r
  where
    s x w = x > (fromIntegral w)/2

scores :: SF (Event Score) (Int, Int)
scores = proc s -> do
  sc <- accumHoldBy s' (0, 0) -< s
  returnA -< sc
  where
    s' (a, b) (Left _) = (a+1, b)
    s' (a, b) (Right _) = (a, b+1)

paddle' p = paddle (PaddleState p 200 (V2 5 60) black)

paddle1 :: SF GameInput PaddleState
paddle1 = paddle' (V2 200 0) paddle1Input

paddle2 :: SF GameInput PaddleState
paddle2 = paddle' (V2 (-200) 0) paddle2Input

ball = ball' (BallState (V2 0 0) (V2 200 400) black)

parseGameInput :: GameInput -> InputEvent -> GameInput
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyUp) G.Down _ _)   = gi { keyUp = G.Down }
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyUp) G.Up _ _)     = gi { keyUp = G.Up }
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyDown) G.Down _ _) = gi { keyDown = G.Down }
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyDown) G.Up _ _)   = gi { keyDown = G.Up }
parseGameInput gi (G.EventKey (G.Char 'w') G.Down _ _) = gi { keyW = G.Down }
parseGameInput gi (G.EventKey (G.Char 'w') G.Up _ _)   = gi { keyW = G.Up }
parseGameInput gi (G.EventKey (G.Char 's') G.Down _ _) = gi { keyS = G.Down }
parseGameInput gi (G.EventKey (G.Char 's') G.Up _ _)   = gi { keyS = G.Up }
parseGameInput gi (G.EventResize (x, y)) = gi {screenSize = V2 x y}
parseGameInput gi _ = gi

input :: SF (Event InputEvent) GameInput
input = accumHoldBy parseGameInput $ GameInput G.Up G.Up G.Up G.Up (V2 100 100)

drawScore :: (Int, Int) -> Picture
-- drawScore (l, r) | (trace (show l ++ " " ++ show r) False) = undefined
drawScore (l, r) = Pictures [Translate 50 0 $ text (show r), Translate (-100) 0 $ text (show l)]

game :: SF GameInput Picture
game = proc gi -> do
  p1 <- paddle1 -< gi
  p2 <- paddle2 -< gi
  rec
    s <- score -< (gi, b)
    s' <- iPre NoEvent -< s `tag` ()
    -- TODO: why does `score` need to be delayed with `pre`
    c <- ballCollision -< (gi, p1, p2, b, s)
    b <- ball -< BallInput c s'
  sc <- scores -< s
  returnA -< Pictures [(drawBall b), (drawPaddle p1), (drawPaddle p2), (drawScore sc)]

defaultPlay :: SF (Event InputEvent) Picture -> IO ()
defaultPlay = playYampa (InWindow "Pong" (500, 250) (200, 200)) white 60

main :: IO ()
main = defaultPlay $ input >>> game

