{-# LANGUAGE Arrows #-}

import Control.Arrow                      ( returnA, (>>>), (^>>), (>>^), (***), (&&&), arr, first, second )
import FRP.Yampa                          ( SF, Event (Event, NoEvent), VectorSpace((*^))
                                          , tag, catEvents, accumHold, mergeBy, after, repeatedly
                                          , accumHoldBy, edgeTag, gate, tagWith, attach
                                          , edge, iPre, integral, hold, pSwitch, iPre, notYet
                                          , drSwitch, dropEvents, par, constant, kSwitch )
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

data VelDirection = VelForward | VelZero | VelBackward deriving (Show, Eq)

data GameInput = GameInput {
  keyLeft :: G.KeyState,
  keyRight :: G.KeyState,
  screenSize :: V2 Int
} deriving Show

rkSwitch :: SF a b -> SF (a, Event (SF a b -> SF a b)) b
rkSwitch i = kSwitch (first i >>^ fst) event' cont
  where
    event' = arr (snd . fst) >>> notYet
    cont sf f = rkSwitch $ f $ (\a -> (a, NoEvent)) ^>> sf

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

type Rocket a = SF (Event a) (Maybe RocketMink)

untilA :: SF a b -> SF (a, Event ()) (Maybe b)
untilA sf = (fmap (tagWith (constant Nothing))) ^>> drSwitch (sf >>^ Just)

untilA' :: SF () b -> SF (Event ()) (Maybe b)
untilA' sf = ((,) ()) ^>> untilA sf

pCat :: a -> [SF a b] -> SF [a] [b]
pCat a sfs = par (\es -> zip (es ++ repeat a)) sfs

pSwitchCatMaybes :: a -> [SF a (Maybe b)] -> SF [a] [b]
pSwitchCatMaybes a sfs = par' sfs >>^ catMaybes
  where
    par' sfs' = pSwitch route sfs' kill cont
    route es = zip (es ++ repeat a)
    kill = arr (coll . (fmap (not . null)) . snd)
    coll bs = bool (Event bs) NoEvent (and bs)
    -- cont _ _ | (trace ("pSwitchCatMaybes cont") False) = undefined
    -- cont _ ks | (trace (show ks) False) = undefined
    cont sfs' keeps = par' $ filterByList keeps sfs'

pSwitchCatEvents :: [SF (Event a) (Maybe b)] -> SF [Event a] [b]
pSwitchCatEvents = pSwitchCatMaybes NoEvent

-- TODO switch to just a or b when the other stops producing results
pSwitchCat :: SF [a] [b] -> SF [a] [b] -> SF [a] [b]
pSwitchCat f s = split f s >>^ uncurry (++)
  where
    split f' s' = proc as -> do
      bs1 <- f -< as
      bs2 <- s -< drop (length bs1) as
      returnA -< (bs1, bs2)

rockets :: [Rocket a] -> SF ([Event a], Event [Rocket a]) [RocketMink]
rockets rs = second spawn >>> rkSwitch (pSwitchCatEvents rs)
  where
    spawn = arr (fmap $ (flip pSwitchCat) . pSwitchCatEvents)

rocket' :: Pos -> Rocket ()
rocket' p = untilA' $ constant (V2 0 20) >>> position p >>> collisionRectangle (V2 20 20)

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
    s               <- repeatedly 5 [rocket' (V2 0 0), rocket' (V2 50 50)]  -< ()
    rs              <- rockets [rocket' (V2 50 50), rocket' (V2 0 0)]       -< ([k] ++ repeat NoEvent, s)
  returnA -< Pictures $ [ drawRectangle ps ] ++ (drawRectangle <$> fst <$> rs)

defaultPlay :: SF (Event InputEvent) Picture -> IO ()
defaultPlay = playYampa (InWindow "Space Invaders" (300, 500) (200, 200)) black 60

main :: IO ()
main = defaultPlay $ input >>> game'
