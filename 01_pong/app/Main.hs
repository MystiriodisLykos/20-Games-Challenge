{-# LANGUAGE Arrows #-}

import Control.Concurrent
import Control.Arrow                      ( returnA, (>>>), arr, (&&&) )
import FRP.Yampa                          ( SF, Event (Event, NoEvent)
                                          , integral, reactimate, now
                                          , constant, event, tag, hold
                                          , mapFilterE, dHold, identity
                                          , rSwitch, accumHoldBy)
import Graphics.Gloss                     ( Display (InWindow)
                                          , Picture (Color,Translate)
                                          , circleSolid
                                          , white
                                          , display
                                          )
import Graphics.Gloss.Interface.FRP.Yampa ( InputEvent, playYampa )
import Debug.Trace (trace)

import qualified Graphics.Gloss.Interface.IO.Game as G


data GameInput = GameInput {
  keyUp :: G.KeyState,
  keyDown :: G.KeyState
}

data PaddleInput = PaddleUp | PaddleDown | PaddleStop

data PaddleState = PaddleState {
  px :: Double,
  py :: Double,
  pv :: Double
}

type PaddleProperties = PaddleState

type Paddle = SF PaddleInput PaddleState

paddle :: PaddleProperties -> Paddle
paddle p = proc pi -> do
  y <- integral -< (pv p) * v pi
  returnA -< p {py = y + py p}
  where
    v PaddleUp = 1
    v PaddleStop = 0
    v PaddleDown = -1

drawPaddle :: PaddleState -> Picture
drawPaddle ps = Translate (realToFrac $ px ps) (realToFrac $ py ps) $ circleSolid 10

animateBall :: SF GameInput Picture
animateBall = proc i -> do
  pi <- paddleInput -< i
  p <- paddle $ PaddleState 0 0 200 -< pi
  returnA -< drawPaddle p

parseGameInput :: GameInput -> InputEvent -> GameInput
--parseGameInput _ i | trace (show i) False = undefined
parseGameInput gi (G.EventKey k@(G.SpecialKey G.KeyUp) G.Down _ _)   = gi { keyUp = G.Down }
parseGameInput gi (G.EventKey k@(G.SpecialKey G.KeyUp) G.Up _ _)     = gi { keyUp = G.Up }
parseGameInput gi (G.EventKey k@(G.SpecialKey G.KeyDown) G.Down _ _) = gi { keyDown = G.Down }
parseGameInput gi (G.EventKey k@(G.SpecialKey G.KeyDown) G.Up _ _)   = gi { keyDown = G.Up }
parseGameInput gi _ = gi

parsePaddleInput :: GameInput -> PaddleInput
parsePaddleInput GameInput{keyUp = G.Down, keyDown = G.Up}   = PaddleUp
parsePaddleInput GameInput{keyUp = G.Down, keyDown = G.Down} = PaddleStop
parsePaddleInput GameInput{keyUp = G.Up, keyDown = G.Down}   = PaddleDown
parsePaddleInput _ = PaddleStop

paddleInput = arr parsePaddleInput

input :: SF (Event InputEvent) GameInput
input = accumHoldBy parseGameInput $ GameInput G.Up G.Up

defaultPlay :: SF (Event InputEvent) Picture -> IO ()
defaultPlay = playYampa (InWindow "YampaDemo" (1280, 1050) (200, 200)) white 60

main :: IO ()
main = defaultPlay $ input >>> animateBall

