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
  screenSize :: V2 Int
} deriving Show

paddleInput :: SF (GameInput, PaddleState) PaddleInput
paddleInput = proc (gi, ps) -> do
  -- TODO: stop paddle when it reaches the top of the screen
  returnA -< PaddleInput $ d gi
  where
    d GameInput{keyUp = G.Down, keyDown = G.Up}   = PaddleUp
    d GameInput{keyUp = G.Down, keyDown = G.Down} = PaddleStop
    d GameInput{keyUp = G.Up, keyDown = G.Down}   = PaddleDown
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
  c <- edgeTag horizontal -< fromMaybe False $ collision 10 p b
  returnA -< c

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
  s' <- iPre NoEvent -< s
  -- TODO: why does `score` need to be delayed with `pre` when the ball function
  -- delays the score with `notYet`
  returnA -< BallInput (merge [wc, pc, s `tag` horizontal]) s'
  where
    merge :: [Event Bounce] -> Event Bounce
    merge = (fmap $ foldl1 (.)) . catEvents

ball' :: SF (GameInput, PaddleState) BallState
ball' = ball (BallState (V2 0 0) (V2 100 200) black) ballInput

paddle' :: SF GameInput PaddleState
paddle' = paddle (PaddleState (V2 200 0) 200 (V2 5 60) black) paddleInput

parseGameInput :: GameInput -> InputEvent -> GameInput
-- parseGameInput gi i | trace ((show gi) ++ " " ++ show i) False = undefined
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyUp) G.Down _ _)   = gi { keyUp = G.Down }
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyUp) G.Up _ _)     = gi { keyUp = G.Up }
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyDown) G.Down _ _) = gi { keyDown = G.Down }
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyDown) G.Up _ _)   = gi { keyDown = G.Up }
parseGameInput gi (G.EventResize (x, y)) = gi {screenSize = V2 x y}
parseGameInput gi _ = gi

input :: SF (Event InputEvent) GameInput
input = accumHoldBy parseGameInput $ GameInput G.Up G.Up (V2 100 100)

game :: SF GameInput Picture
game = proc gi -> do
  p <- paddle' -< gi
  b <- ball' -< (gi, p)
  returnA -< Pictures [(drawBall b), (drawPaddle p)]

defaultPlay :: SF (Event InputEvent) Picture -> IO ()
defaultPlay = playYampa (InWindow "YampaDemo" (500, 250) (200, 200)) white 60

main :: IO ()
main = defaultPlay $ input >>> game

