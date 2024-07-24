{-# LANGUAGE Arrows #-}

import Control.Arrow                      ( returnA, (>>>), (^>>), (>>^), (***), (&&&), arr, first, second )
import FRP.Yampa                          ( SF, Event (Event, NoEvent), VectorSpace((*^))
                                          , tag, catEvents, accumHold, mergeBy, after, repeatedly
                                          , accumHoldBy, edgeTag, gate, tagWith, attach, dSwitch
                                          , edge, iPre, integral, hold, pSwitch, iPre, notYet
                                          , drSwitch, dropEvents, par, constant, kSwitch )
import FRP.Yampa.Switches                 ( drpSwitchZ)
import Graphics.Gloss                     ( Display (InWindow)
                                          , Picture (Pictures, Translate)
                                          , circleSolid, polygon
                                          , white, black, color
                                          )
import Graphics.Gloss.Interface.FRP.Yampa ( InputEvent, playYampa )
import Linear.V2 (V2 (V2), perp)
import Linear.Metric (dot, norm)
import Linear.Vector ((^/))
import GJK.Collision (collision)
import GJK.Mink (Mink)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Bool (bool)
import GHC.Float (double2Float)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Debug.Trace (trace)

import Linear.GJK (minkCircle, minkRectangle, minkSegment)
import Linear.VectorSpace ()

type Pos = V2 Double
type Vel = V2 Double
type Size = V2 Double

type CircleMink = Mink (Double, Pos)
type RectangleMink = Mink [V2 Double]
type PaddleMink = RectangleMink
type RocketMink = RectangleMink

type ScreenSize = V2 Int

type KSF a b = SF a (Maybe b)
type Rocket a = SF (Event a) (Maybe RocketMink)

data VelDirection = VelForward | VelZero | VelBackward deriving (Show, Eq)

data GameInput = GameInput {
  keyLeft :: G.KeyState,
  keyRight :: G.KeyState,
  screenSize :: V2 Int
} deriving Show

arrUntil :: SF a b -> SF (a, Event c) (Maybe b)
arrUntil sf = dSwitch (first $ sf >>^ Just) (const $ constant Nothing)

arrUntilEvent :: SF () b -> SF (Event ()) (Maybe b)
arrUntilEvent sf = ((,) ()) ^>> arrUntil sf

drpKillSwitchZ :: a -> [KSF a b] -> SF ([a], Event ([KSF a b] -> [KSF a b])) [b]
drpKillSwitchZ a sfs = proc (as, e) -> do
  rec
    bs <- drpSwitchZ sfs -< (as ++ repeat a, mergeC [kill, e])
    kill <- kill' -< bs
  returnA -< catMaybes bs
  where
    kill' = arr (coll . (fmap (not . null)))
    coll bs = bool (Event $ filterByList $ bs ++ repeat True) NoEvent (and bs)

spawn :: SF (Event ([KSF a b])) (Event ([KSF a b] -> [KSF a b]))
spawn = arr $ fmap $ flip (++)

pKillSpawnZ :: a -> [KSF a b] -> SF ([a], Event ([KSF a b])) [b]
pKillSpawnZ a sfs = second spawn >>> drpKillSwitchZ a sfs

mergeC :: [Event (a -> a)] -> Event (a -> a)
mergeC = (fmap $ foldl1 (.)) . catEvents

position :: Pos -> SF Vel Pos
position p0 = integral >>^ ((+) p0)

lVelocity :: Vel -> SF VelDirection Vel
lVelocity v = arr (\d -> (d' d) * v)
  where
    d' VelForward = 1
    d' VelBackward = -1
    d' _ = 0

filterByList :: [Bool] -> [a] -> [a]
-- filterByList (b:bs) _ | (trace (show b) False) = undefined
filterByList (True:bs)  (x:xs) = x : filterByList bs xs
filterByList (False:bs) (_:xs) =     filterByList bs xs
filterByList _          _      = []

collision' :: (Mink a, Mink b) -> Bool
collision' = (fromMaybe False) . uncurry (collision 10)

-- flip size and position arguments
minkRectangle' :: V2 Double -> V2 Double -> Mink [V2 Double]
minkRectangle' s p = minkRectangle p s

collisionRectangle :: V2 Double -> SF Pos (Mink [V2 Double])
collisionRectangle s = arr $ minkRectangle' s

drawRectangle :: [V2 Double] -> Picture
drawRectangle = color white . polygon . fmap (\(V2 x y) -> (double2Float x, double2Float y))

paddle :: SF VelDirection PaddleMink
paddle = lVelocity (V2 100 0) >>> (position $ V2 0 (-200)) >>> (collisionRectangle $ V2 50 5)

rocket :: Pos -> Rocket ()
rocket p = arrUntilEvent $ constant (V2 0 20) >>> position p >>> collisionRectangle (V2 20 20)

rockets :: [Rocket a] -> SF ([Event a], Event [Rocket a]) [RocketMink]
rockets = pKillSpawnZ NoEvent

countDown :: Int -> SF (Event ()) (Event ())
countDown n = proc e -> do
  i <- accumHold n -< e `tag` ((+) (-1))
  e' <- edge -< i <= 0
  returnA -< e'

parseGameInput :: GameInput -> InputEvent -> GameInput
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyLeft) G.Down _ _)   = gi { keyLeft = G.Down }
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyLeft) G.Up _ _)     = gi { keyLeft = G.Up }
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyRight) G.Down _ _) = gi { keyRight = G.Down }
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyRight) G.Up _ _)   = gi { keyRight = G.Up }
parseGameInput gi (G.EventResize (x, y)) = gi {screenSize = V2 x y}
parseGameInput gi _ = gi

input :: SF (Event InputEvent) GameInput
input = accumHoldBy parseGameInput $ GameInput G.Up G.Up (V2 100 100)

paddleD :: G.KeyState -> G.KeyState -> VelDirection
paddleD G.Down G.Up = VelForward
paddleD G.Up G.Down = VelBackward
paddleD _ _ = VelZero

game' :: SF GameInput Picture
game' = proc gi -> do
  rec
    p@(ps, _)       <- paddle                        -< paddleD (keyRight gi) (keyLeft gi)
    k               <- repeatedly 2.5 ()                   -< ()
    s               <- repeatedly 5 [rocket (V2 0 0), rocket (V2 50 50)]  -< ()
    rs              <- rockets [rocket (V2 50 50), rocket (V2 0 0)]       -< ([k], s)
  returnA -< Pictures $ [ drawRectangle ps ] ++ (drawRectangle <$> fst <$> rs)

defaultPlay :: SF (Event InputEvent) Picture -> IO ()
defaultPlay = playYampa (InWindow "Space Invaders" (300, 500) (200, 200)) black 60

main :: IO ()
main = defaultPlay $ input >>> game'
