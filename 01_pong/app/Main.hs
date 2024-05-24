{-# LANGUAGE Arrows #-}

import Control.Concurrent
import Control.Arrow                      ( returnA, (>>>), arr, (&&&) )
import FRP.Yampa                          ( SF, Event (Event, NoEvent)
                                          , integral, reactimate, now
                                          , constant, event, tag, hold
                                          , mapFilterE, dHold, identity
                                          , rSwitch, accumHoldBy, edgeTag
                                          , repeatedly, edge, notYet, pre, never)
import Graphics.Gloss                     ( Display (InWindow)
                                          , Picture (Color,Translate,Pictures)
                                          , circleSolid
                                          , white
                                          , black
                                          , display
                                          )
import Graphics.Gloss.Interface.FRP.Yampa ( InputEvent, playYampa )
import Debug.Trace (trace)

import qualified Graphics.Gloss.Interface.IO.Game as G
import Linear.V2   ( V2 (V2) )
import Control.Applicative ((<|>))

import Ball (BallState(..), Collision, Score, vertical, horizontal, ball, drawBall)
import Types (v2x, v2y)
import Paddle (PaddleDirection(..), PaddleInput(..), PaddleState(PaddleState, pP), paddle, drawPaddle)

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

wallCollision :: Collision (GameInput, BallState)
wallCollision = proc (gi, bs) -> do
  let y = (fromIntegral . v2y . screenSize) gi
  c <- edgeTag vertical -< abs (v2y $ bP bs) + 10 >= y/2
  returnA -< c

score :: Score (GameInput, BallState)
score = proc (gi, bs) -> do
  let w = (fromIntegral . v2x . screenSize) gi
      x = (abs (v2x $ bP bs)) - 10
  s <- edge -< x >= w/2
  returnA -< s

ball' :: SF GameInput BallState
ball' = ball (BallState (V2 0 0) (V2 50 100) black) wallCollision (score >>> pre)
-- TODO: why does `score` need to be delayed with `pre` when the ball function
-- delays the score with `notYet`

paddle' = paddle (PaddleState (V2 0 0) 200 (V2 20 60) black) paddleInput

parseGameInput :: GameInput -> InputEvent -> GameInput
-- parseGameInput gi i | trace ((show gi) ++ " " ++ show i) False = undefined
parseGameInput gi (G.EventKey k@(G.SpecialKey G.KeyUp) G.Down _ _)   = gi { keyUp = G.Down }
parseGameInput gi (G.EventKey k@(G.SpecialKey G.KeyUp) G.Up _ _)     = gi { keyUp = G.Up }
parseGameInput gi (G.EventKey k@(G.SpecialKey G.KeyDown) G.Down _ _) = gi { keyDown = G.Down }
parseGameInput gi (G.EventKey k@(G.SpecialKey G.KeyDown) G.Up _ _)   = gi { keyDown = G.Up }
parseGameInput gi (G.EventResize (x, y)) = gi {screenSize = V2 x y}
parseGameInput gi _ = gi

input :: SF (Event InputEvent) GameInput
input = accumHoldBy parseGameInput $ GameInput G.Up G.Up (V2 100 100)

game :: SF GameInput Picture
game = proc gi -> do
  b <- ball' -< gi
  p <- paddle' -< gi
  returnA -< Pictures [(drawBall b), (drawPaddle p)]

defaultPlay :: SF (Event InputEvent) Picture -> IO ()
defaultPlay = playYampa (InWindow "YampaDemo" (1280, 1050) (200, 200)) white 60

main :: IO ()
main = defaultPlay $ input >>> game

