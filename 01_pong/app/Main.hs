{-# LANGUAGE Arrows #-}

import Control.Arrow                      ( returnA, (>>>) )
import FRP.Yampa                          ( SF, Event (NoEvent)
                                          , tag, catEvents
                                          , accumHoldBy, edgeTag, mergeBy
                                          , edge, iPre)
import Graphics.Gloss                     ( Display (InWindow)
                                          , Picture (Pictures)
                                          , white
                                          , black
                                          )
import Graphics.Gloss.Interface.FRP.Yampa ( InputEvent, playYampa )
import Linear.V2 (V2 (V2))
import GJK.Collision (collision)
import Data.Maybe (fromMaybe)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Debug.Trace (trace)

import Ball (BallState(..), BallInput(..), Bounce, vertical, horizontal, ball, drawBall)
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

score :: SF (GameInput, BallState) (Event ())
score = proc (gi, bs) -> do
  let
    (V2 w _) = screenSize gi
    (V2 x _) = bP bs
  s <- edge -< (abs x) - 10 >= (fromIntegral w)/2
  returnA -< s

ballInput :: SF ((GameInput, PaddleState), BallState) BallInput
ballInput = proc ((gi, ps), bs) -> do
  wc <- wallCollision -< (gi, bs)
  pc <- paddleCollision -< (ps, bs)
  s <- score -< (gi, bs)
  s' <- iPre NoEvent -< s `tag` ()
  -- TODO: why does `score` need to be delayed with `pre`
  returnA -< BallInput (merge [wc, pc, s `tag` horizontal]) s'
  where
    merge :: [Event Bounce] -> Event Bounce
    merge = (fmap $ foldl1 (.)) . catEvents

ball' :: SF (GameInput, PaddleState) BallState
ball' = ball (BallState (V2 0 0) (V2 200 400) black) ballInput

paddle' p = paddle (PaddleState p 200 (V2 5 60) black)

paddle1 :: SF GameInput PaddleState
paddle1 = paddle' (V2 200 0) paddle1Input

paddle2 :: SF GameInput PaddleState
paddle2 = paddle' (V2 (-200) 0) paddle2Input

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

game :: SF GameInput Picture
game = proc gi -> do
  p1 <- paddle1 -< gi
  p2 <- paddle2 -< gi
  rec
    b <- ball' -< (gi, if (bP b >= V2 0 0) then p1 else p2)
  returnA -< Pictures [(drawBall b), (drawPaddle p1), (drawPaddle p2)]

defaultPlay :: SF (Event InputEvent) Picture -> IO ()
defaultPlay = playYampa (InWindow "Pong" (500, 250) (200, 200)) white 60

main :: IO ()
main = defaultPlay $ input >>> game

