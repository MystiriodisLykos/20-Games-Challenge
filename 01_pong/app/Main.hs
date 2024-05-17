{-# LANGUAGE Arrows #-}

import Control.Concurrent
import Control.Arrow                      ( returnA, (>>>) )
import FRP.Yampa                          ( SF, Event (Event)
                                          , integral, reactimate, now )
import Graphics.Gloss                     ( Display (InWindow)
                                          , Picture (Color,Translate)
                                          , circleSolid
                                          , white
                                          , display
                                          )
import Graphics.Gloss.Interface.FRP.Yampa ( InputEvent, playYampa )

import qualified Graphics.Gloss.Interface.IO.Game as G

type Pos = Double
type Vel = Double

data Direction = Pressed | None

fallingBall :: Pos -> SF a (Pos, Vel)
fallingBall y0 = proc _ -> do
  v <- integral -< -9.81
  y <- integral -< v
  returnA -< (y + y0, v)

drawBall :: Pos -> Picture
drawBall p = Translate 0 (realToFrac p) $ circleSolid 10

animateBall :: SF a Picture
animateBall = proc _ -> do
  (pos, vel) <- fallingBall 0 -< ()
  returnA -< drawBall pos

parseInput :: SF (Event InputEvent) (Event Direction)
parseInput = proc e -> case e of
    Event (G.EventKey (G.SpecialKey G.KeyUp) G.Down _ _) -> now Pressed -< ()
    _ -> now None -< ()

defaultPlay :: SF (Event InputEvent) Picture -> IO ()
defaultPlay = playYampa (InWindow "YampaDemo" (1280, 1050) (200, 200)) white 30

main :: IO ()
main = defaultPlay $ animateBall
--main = reactimate (return ())
--                  (\_ -> threadDelay 100000 >> return (0.1, Nothing))
--                  (\_ (pos, vel) -> putStrLn ("pos: " ++ show pos ++ " vel: " ++ show vel) >> return False)
--                  (fallingBall 10)
