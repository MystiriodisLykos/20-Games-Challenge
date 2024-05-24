{-# LANGUAGE Arrows #-}

module Paddle( PaddleDirection(..)
             , PaddleInput(..)
             , PaddleState(..)
             , paddle
             , drawPaddle
             ) where

import Control.Concurrent
import Control.Arrow                      ( returnA, (>>>), arr, (&&&) )
import FRP.Yampa                          ( SF, Event (Event, NoEvent)
                                          , integral, reactimate, now
                                          , constant, event, tag, hold
                                          , mapFilterE, dHold, identity
                                          , rSwitch, accumHoldBy, edgeTag
                                          , repeatedly, edge, notYet, pre, never)
import Graphics.Gloss                     ( Display (InWindow)
                                          , Picture (Color,Translate)
                                          , Color
                                          , rectangleSolid
                                          , white
                                          , black
                                          , display
                                          )
import Graphics.Gloss.Interface.FRP.Yampa ( InputEvent, playYampa )
import Debug.Trace (trace)

import qualified Graphics.Gloss.Interface.IO.Game as G
import Linear.V2   ( V2 (V2) )

import Types (v2x, v2y)

data PaddleDirection = PaddleUp | PaddleDown | PaddleStop deriving Show

data PaddleInput = PaddleInput {
  paddleDirection :: PaddleDirection
}

data PaddleState = PaddleState {
  pP :: V2 Float,
  pV :: Float,
  pS :: V2 Float,
  pColor :: Color
}

paddle' :: PaddleState -> SF PaddleInput PaddleState
paddle' initial = proc pi -> do
  y <- integral -< (pV initial) * (v $ paddleDirection pi)
  returnA -< initial {pP = (V2 0 y) + pP initial}
  where
    v PaddleUp = 1
    v PaddleStop = 0
    v PaddleDown = -1

paddle :: PaddleState -> SF (a, PaddleState) PaddleInput -> SF a PaddleState
paddle initial pi = proc i -> do
  rec
    pi <- pi -< (i, ps)
    ps <- paddle' initial -< pi
  returnA -< ps

drawPaddle :: PaddleState -> Picture
drawPaddle (PaddleState (V2 x y) _ (V2 w h) color) = Color color $ Translate x y $ rectangleSolid w h

-- Example

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

defaultPlay :: SF (Event InputEvent) Picture -> IO ()
defaultPlay = playYampa (InWindow "YampaDemo" (1280, 1050) (200, 200)) white 60

main :: IO ()
main = defaultPlay $ input >>> examplePaddle >>> (arr drawPaddle)

