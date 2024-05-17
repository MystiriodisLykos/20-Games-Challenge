{-# LANGUAGE Arrows #-}

import Control.Concurrent
import Control.Arrow                      ( returnA, (>>>), arr )
import FRP.Yampa                          ( SF, Event (Event, NoEvent)
                                          , integral, reactimate, now
                                          , constant, event, tag, hold
                                          , mapFilterE, dHold)
import Graphics.Gloss                     ( Display (InWindow)
                                          , Picture (Color,Translate)
                                          , circleSolid
                                          , white
                                          , display
                                          )
import Graphics.Gloss.Interface.FRP.Yampa ( InputEvent, playYampa )
import Debug.Trace (trace)

import qualified Graphics.Gloss.Interface.IO.Game as G

data KeyInput = KeyInput {
  key :: G.Key,
  state :: G.KeyState
}

data PaddleInput = MoveUp | MoveDown | Stop deriving Show

instance Semigroup PaddleInput where
  --(<>) a b | (trace ((show a) ++ (show b)) False) = undefined
  (<>) MoveUp MoveDown = Stop
  (<>) MoveDown MoveUp = Stop
  (<>) Stop a = a
  (<>) a Stop = a
  (<>) MoveUp _ = MoveUp
  (<>) MoveDown _ = MoveDown

instance Semigroup a => Semigroup (Event a) where
  (<>) (Event a) (Event b) = Event (a <> b)
  (<>) NoEvent b = b
  (<>) a NoEvent = a

type GameInput = Event KeyInput

data PaddleState = PaddleState {
  x :: Double,
  y :: Double
} deriving Show

type Paddle = SF PaddleInput PaddleState

paddle :: Double -> Double -> Paddle
paddle x0 y0 = proc pi -> do
  y <- integral -< (paddleV pi)
  returnA -< PaddleState x0 (y0 + y)
  where
    paddleV MoveUp = 100
    paddleV MoveDown = -100
    paddleV Stop = 0

drawBall :: Double -> Picture
drawBall p = Translate 0 (realToFrac p) $ circleSolid 10

animateBall :: SF GameInput Picture
animateBall = proc i -> do
  rec
    pi <- dHold Stop -< (fmap parsePaddleInput i) <> Event pi
    ps <- paddle 0 0 -< pi
  returnA -< drawBall (y ps)

parseGlossInput :: InputEvent -> Maybe KeyInput
--parseGlossInput i | trace (show i) False = undefined
parseGlossInput (G.EventKey k@(G.SpecialKey G.KeyUp) G.Down _ _) = Just (KeyInput k G.Down)
parseGlossInput (G.EventKey k@(G.SpecialKey G.KeyUp) G.Up _ _) = Just (KeyInput k G.Up)
parseGlossInput (G.EventKey k@(G.SpecialKey G.KeyDown) G.Down _ _) = Just (KeyInput k G.Down)
parseGlossInput (G.EventKey k@(G.SpecialKey G.KeyDown) G.Up _ _) = Just (KeyInput k G.Up)
parseGlossInput _ = Nothing

parsePaddleInput :: KeyInput -> PaddleInput
parsePaddleInput (KeyInput (G.SpecialKey G.KeyUp)   G.Down) = MoveUp
parsePaddleInput (KeyInput (G.SpecialKey G.KeyUp)   G.Up  ) = MoveDown
parsePaddleInput (KeyInput (G.SpecialKey G.KeyDown) G.Down) = MoveDown
parsePaddleInput  (KeyInput (G.SpecialKey G.KeyDown) G.Up  ) = MoveUp

parseInput :: SF (Event InputEvent) GameInput
parseInput = proc e ->
  returnA -< mapFilterE parseGlossInput e

defaultPlay :: SF (Event InputEvent) Picture -> IO ()
defaultPlay = playYampa (InWindow "YampaDemo" (1280, 1050) (200, 200)) white 30

main :: IO ()
main = defaultPlay $ parseInput >>> animateBall
--main = reactimate (return $ Event (G.EventKey (G.SpecialKey G.KeyLeft) G.Down (G.Modifiers G.Up G.Up G.Up) (1,1)))
--                  (\_ -> threadDelay 10000 >> return (0.1, Just (Event $ G.EventKey (G.SpecialKey G.KeyUp) G.Down (G.Modifiers G.Up G.Up G.Up) (1,1))))
--                  (\_ -> return (0.1,  Nothing))
--                  (\_ e -> putStrLn (show e) >> return False)
--                  (parseInput)
--                  (\_ (pos, vel) -> putStrLn ("pos: " ++ show pos ++ " vel: " ++ show vel) >> return False)
--                  (fallingBall 10)
