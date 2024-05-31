{-# LANGUAGE Arrows #-}

import Control.Arrow
import FRP.Yampa
import Graphics.Gloss
import Linear.V2
import Graphics.Gloss.Interface.FRP.Yampa ( InputEvent, playYampa )
import qualified Graphics.Gloss.Interface.IO.Game as G

import Ball
import Paddle

-- Ball

exampleBallCollision :: SF BallState (Event Bounce)
exampleBallCollision = proc bs -> do
  let (V2 x y) = bP bs
  vc <- edgeTag vertical -< abs y >= 200
  hc <- edgeTag horizontal -< abs x >= 200
  returnA -< mergeBy (.) vc hc

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

-- Paddle

examplePaddle = paddle (PaddleState (V2 0 0) 200 (V2 20 60) black) examplePaddleInput

data GameInput = GameInput {
  keyUp :: G.KeyState,
  keyDown :: G.KeyState
} deriving Show

examplePaddleInput :: SF (GameInput, a) PaddleInput
examplePaddleInput = proc (gi, _) -> do
  returnA -< PaddleInput $ d gi
  where
    d GameInput{keyUp = G.Down, keyDown = G.Up}   = PaddleUp
    d GameInput{keyUp = G.Down, keyDown = G.Down} = PaddleStop
    d GameInput{keyUp = G.Up, keyDown = G.Down}   = PaddleDown
    d _ = PaddleStop

parseGameInput :: GameInput -> InputEvent -> GameInput
-- parseGameInput gi i | trace ((show gi) ++ " " ++ show i) False = undefined
parseGameInput gi (G.EventKey k@(G.SpecialKey G.KeyUp) G.Down _ _)   = gi { keyUp = G.Down }
parseGameInput gi (G.EventKey k@(G.SpecialKey G.KeyUp) G.Up _ _)     = gi { keyUp = G.Up }
parseGameInput gi (G.EventKey k@(G.SpecialKey G.KeyDown) G.Down _ _) = gi { keyDown = G.Down }
parseGameInput gi (G.EventKey k@(G.SpecialKey G.KeyDown) G.Up _ _)   = gi { keyDown = G.Up }
parseGameInput gi _ = gi

input :: SF (Event InputEvent) GameInput
input = accumHoldBy parseGameInput $ GameInput G.Up G.Up

-- Main

main :: IO ()

-- Ball
-- main = defaultPlay $ parB ((\b -> b >>> (arr drawBall)) <$> [
--   exampleBall black (V2 50 (-50)),
--   exampleBall red (V2 50   50 ),
--   exampleBall blue (V2 50  100 ),
--   exampleBall green (V2 50 (-100)),
--   exampleBall orange (V2 0  (-100)),
--   exampleBall yellow (V2 50 0 )
--   ]) >>> arr Pictures

-- Paddle
main = defaultPlay $ input >>> examplePaddle >>> (arr drawPaddle)

