{-# LANGUAGE Arrows #-}

import Control.Concurrent
import Control.Arrow                      ( returnA, (>>>), arr )
import FRP.Yampa                          ( SF, Event (Event, NoEvent)
                                          , integral, reactimate, now
                                          , constant, event, tag, hold
                                          , mapFilterE)
import Graphics.Gloss                     ( Display (InWindow)
                                          , Picture (Color,Translate)
                                          , circleSolid
                                          , white
                                          , display
                                          )
import Graphics.Gloss.Interface.FRP.Yampa ( InputEvent, playYampa )
import Debug.Trace (trace)

import qualified Graphics.Gloss.Interface.IO.Game as G

type Pos = Double
type Vel = Double
type Acc = Double

data Direction = Up | Down deriving Show
type GameInput = Event Direction

acceleration :: SF Direction Acc
acceleration = proc d -> case d of
  Up -> constant 98 -< ()
  _ -> constant (-98) -< ()

fallingBall :: Pos -> SF Direction (Pos, Vel)
fallingBall y0 = proc d -> do
  a <- acceleration -< d
  v <- integral -< a
  y <- integral -< v
  returnA -< (y + y0, v)

drawBall :: Pos -> Picture
drawBall p = Translate 0 (realToFrac p) $ circleSolid 10

animateBall :: SF Direction Picture
animateBall = proc i -> do
  (pos, vel) <- fallingBall 0 -< i
  returnA -< drawBall pos

parsegInput :: InputEvent -> Maybe Direction
parsegInput (G.EventKey (G.SpecialKey G.KeyUp) G.Down _ _) = Just Up
parsegInput (G.EventKey (G.SpecialKey G.KeyUp) G.Up _ _) = Just Down
parsegInput _ = Nothing

parseInput :: SF (Event InputEvent) GameInput
parseInput = proc e ->
  returnA -< mapFilterE parsegInput e

defaultPlay :: SF (Event InputEvent) Picture -> IO ()
defaultPlay = playYampa (InWindow "YampaDemo" (1280, 1050) (200, 200)) white 30

main :: IO ()
main = defaultPlay $ parseInput >>> (hold Down) >>> animateBall
--main = reactimate (return $ Event (G.EventKey (G.SpecialKey G.KeyLeft) G.Down (G.Modifiers G.Up G.Up G.Up) (1,1)))
--                  (\_ -> threadDelay 10000 >> return (0.1, Just (Event $ G.EventKey (G.SpecialKey G.KeyUp) G.Down (G.Modifiers G.Up G.Up G.Up) (1,1))))
--                  (\_ -> return (0.1,  Nothing))
--                  (\_ e -> putStrLn (show e) >> return False)
--                  (parseInput)
--                  (\_ (pos, vel) -> putStrLn ("pos: " ++ show pos ++ " vel: " ++ show vel) >> return False)
--                  (fallingBall 10)
