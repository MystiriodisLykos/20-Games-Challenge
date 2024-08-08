{-# LANGUAGE Arrows, KindSignatures, MultiParamTypeClasses, ConstraintKinds, FlexibleInstances, FlexibleContexts #-}

import Control.Arrow                      ( returnA, (>>>), (^>>), (>>^), (&&&), (***), arr, first, second )
import Control.Applicative                ( Alternative, empty, (<|>) )
import FRP.Yampa                          ( SF, Event (Event, NoEvent)
                                          , tag, catEvents, accumHold, after, notYet, isEvent
                                          , accumHoldBy, dSwitch, constant, iPre, event
                                          , edge, integral, hold, switch, rSwitch
                                          , iEdge, repeatedly, switch, dropEvents )
import Graphics.Gloss                     ( Display (InWindow)
                                          , Picture (Pictures)
                                          , polygon, scale
                                          , white, black
                                          , green, blue
                                          , red, yellow
                                          , color, text
                                          )
import Graphics.Gloss.Interface.FRP.Yampa ( InputEvent, playYampa )
import Linear.V2 (V2 (V2))
import Linear.Vector (Additive, lerp, zero)
import GJK.Mink (Mink)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Either (isLeft)
import Data.Kind (Constraint)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import GHC.Float (double2Float, int2Float)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Debug.Trace (trace)

import Linear.GJK         ( collision', minkRectangle' )
import Linear.VectorSpace ()
import Data.DMap          (toMap, elems, fromList, partition, DMap (DMap), IMap)
import FRP.Yampa.Game     ( WithKillFlag (..)
                          , switchAfter, onlyEvery
                          , pKillSpawn
                          , switchWhenE
                          , partAliveDeadE
                          )
-- import FRP.Yampa.AltSwitches ( parA )

import Witherable as W

class WithCollision a b where
  collision :: a -> Mink b

class WithScore a where
  score :: a -> Int

class Drawable a where
  draw :: a -> Picture

type AlienType a = ( WithKillFlag a
                    , WithCollision a [V2 Double]
                    , WithScore a
                    , Drawable a) :: Constraint

data BasicAlienType = RedAlien {aDead :: Bool, aPos :: Pos}
                | BlueAlien {aDead :: Bool, aPos :: Pos}
                | GreenAlien {aDead :: Bool, aPos :: Pos}
                | YellowAlien {aDead :: Bool, aPos :: Pos}
                deriving (Show, Eq)

instance WithKillFlag (BasicAlienType) where
  killF = aDead

instance WithCollision BasicAlienType [V2 Double] where
  collision a = minkRectangle' (V2 30 30) (aPos a)

instance WithScore BasicAlienType where
  score (RedAlien _ _) = 1
  score (BlueAlien _ _) = 2
  score (GreenAlien _ _) = 3
  score (YellowAlien _ _) = 4

instance Drawable BasicAlienType where
  draw a = draw' a $ color' a
    where
      draw' a' c = drawRectangle c $ fst $ minkRectangle' (V2 30 30) (aPos a')
      -- color' a | (trace (show a) False) = undefined
      color' (RedAlien _ _) = red
      color' (BlueAlien _ _) = blue
      color' (GreenAlien _ _) = green
      color' (YellowAlien _ _) = yellow

type Pos = V2 Double
type Vel = V2 Double
type Size = V2 Double

type RectangleMink = Mink [V2 Double]
type PaddleMink = RectangleMink
type RocketMink = RectangleMink
type AlienMink = RectangleMink

type ScreenSize = V2 Int

data VelDirection = VelForward | VelZero | VelBackward deriving (Show, Eq)

data GameInput = GameInput {
  keyLeft :: G.KeyState,
  keyRight :: G.KeyState,
  keyFire :: G.KeyState,
  screenSize :: ScreenSize
} deriving Show

type Rocket a = SF (Event a) (Maybe RocketMink)
type Gun a b = SF (Pos, a) (Event [Rocket b])
type Ship = SF VelDirection PaddleMink
type Alien t a = SF (Event a) (t)
type BasicAlien a = Alien BasicAlienType a

avg :: (Fractional a, Additive f) => [f a] -> f a
avg []     = zero
avg [x]    = x
avg (x:xs) = lerp 0.5 x $ avg xs

rightEvent :: Either a b -> Event b
rightEvent (Right b) = Event b
rightEvent _         = NoEvent

leftEvent :: Either a b -> Event a
leftEvent (Left a) = Event a
leftEvent _        = NoEvent

position :: Pos -> SF Vel Pos
position p0 = integral >>^ ((+) p0)

lVelocity :: Vel -> SF VelDirection Vel
lVelocity v = arr (\d -> (d' d) * v)
  where
    d' VelForward = 1
    d' VelBackward = -1
    d' _ = 0

aVelocity :: SF a Vel
aVelocity = lVel
  where
    switch' :: Double -> Vel -> SF a Vel -> SF a Vel
    switch' d v b = switchAfter d (constant v) b
    lVel   = switch' 10  (V2 (-50) 0) (dVel rVel)
    rVel   = switch' 10  (V2   50  0) (dVel lVel)
    dVel n = switch' 0.25 (V2 0 (-50)) n

collisionRectangle :: V2 Double -> SF Pos (Mink [V2 Double])
collisionRectangle s = arr $ minkRectangle' s

ship :: Ship
ship = lVelocity (V2 100 0) >>> (position $ V2 0 (-200)) >>> (collisionRectangle $ V2 50 5)

rocket :: Pos -> Rocket ()
rocket p = switchWhenE $ constant (V2 0 50) >>> position p >>> collisionRectangle (V2 20 20)

rocket' :: Pos -> Rocket ()
rocket' p = fmap Left ^>> vBoundRocket 300 (rocket p)

vBoundRocket :: Double -> Rocket a -> Rocket (Either a Double)
vBoundRocket iTop r = proc e -> do
  r' <- r -< e >>= leftEvent
  top <- hold iTop -< e >>= rightEvent
  returnA -< r' >>= cap top
  where
    cap :: Double -> RocketMink -> Maybe RocketMink
    cap top' (rps, _) | (or $ over top' <$> rps) = Nothing
    cap _    rm                                  = Just rm
    over :: Double -> Pos -> Bool
    over ym (V2 _ y) = y > ym

basicGun :: Gun Bool ()
basicGun = onlyEvery 1 $ second (iEdge False) >>^ (\(p, e) -> e `tag` [rocket' p])

tagOn :: SF a b -> SF (a, Event c) (b, Event b)
tagOn sf = proc (a, e) -> do
  b <- sf -< a
  returnA -< (b, e `tag` b)

tagOnE :: SF () b -> SF (Event c) (b, Event b)
tagOnE sf = ((,) ()) ^>> tagOn sf

alienMovement :: Pos -> SF a Pos
alienMovement i = aVelocity >>> position i

redAlien :: Pos -> BasicAlien a
redAlien i = switch (tagOnE $
                          alienMovement i >>^
                          RedAlien False)
                  (\l -> constant l{aDead=True})

blueAlien :: Pos -> BasicAlien a
blueAlien i = redAlien i >>^ (\(RedAlien a b) -> BlueAlien a b)

greenAlien :: Pos -> BasicAlien a
greenAlien i = redAlien i >>^ (\(RedAlien a b) -> GreenAlien a b)

yellowAlien :: Pos -> BasicAlien a
yellowAlien i = redAlien i >>^ (\(RedAlien a b) -> YellowAlien a b)

alienTypes = [redAlien, greenAlien, blueAlien, yellowAlien]

aliens1 = [c (V2 x y) | x      <- take 9 [50,100..]
                      , (c, y) <- take 4 $ zip alienTypes [100,150..]]

aliens :: AlienType a =>
  [SF (Event c) a] ->
  SF (IMap (Event c), Event [SF (Event c) a]) (IMap a, Event (IMap a))
aliens i = second index
           >>> pKillSpawn NoEvent (fromList i)
           >>> partAliveDeadE

rockets :: SF (IMap (Event a), Event [Rocket a]) (IMap RocketMink)
rockets = second index >>> pKillSpawn NoEvent empty >>^ W.catMaybes

index :: SF (Event [a]) (Event (IMap a))
index = proc as -> do
  rec
    s <- iPre 0 -< e
    e <- accumHoldBy (\p n -> (length n) + p) 0 -< as
  returnA -< (DMap Nothing . Map.fromAscList . zip [s..e]) <$> as

countDown :: Int -> SF (Event ()) (Event ())
countDown n = proc e -> do
  i <- accumHold n -< e `tag` ((+) (-1))
  e' <- edge -< i <= 0
  returnA -< e'

scaleA :: SF GameInput (Picture -> Picture)
scaleA = arr $ (\(V2 x y) -> scale x y) . scale' . size'
  where
    size' = (fmap int2Float) . screenSize
    scale' sizeC = sizeC / size' giI

parseGameInput :: GameInput -> InputEvent -> GameInput
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyLeft) s _ _)   = gi { keyLeft = s }
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyRight) s _ _) = gi { keyRight = s }
parseGameInput gi (G.EventKey (G.SpecialKey G.KeySpace) s _ _) = gi { keyFire = s }
parseGameInput gi (G.EventResize (x, y)) = gi {screenSize = V2 x y}
parseGameInput gi _ = gi

giI = GameInput G.Up G.Up G.Up (V2 1000 600)

input :: SF (Event InputEvent) GameInput
input = accumHoldBy parseGameInput $ giI

shipD :: G.KeyState -> G.KeyState -> VelDirection
shipD G.Down G.Up = VelForward
shipD G.Up G.Down = VelBackward
shipD _ _ = VelZero

collisionTest :: (Show a) => a -> SF (IMap v) (IMap (Event a))
collisionTest a = map' >>> iPre empty
  where
    map' = proc (DMap _ m) -> do
      e <- repeatedly 5 a -< ()
      returnA -< W.filter (isEvent) $ DMap Nothing $ e <$ Map.restrictKeys m (oneKey m)
    oneKey m' = Set.fromList $ take 1 $ Map.keys m'

game' :: SF GameInput Picture
game' = proc gi -> do
  rec
    p@(ps, _)       <- ship             -< shipD (keyRight gi) (keyLeft gi)
    spawnRs         <- basicGun         -< (avg ps, keyFire gi == G.Down)
    ae              <- collisionTest () -< as
    (as, kills)     <- aliens aliens1   -< (ae, NoEvent)
    re              <- collisionTest () -< rs
    rs              <- rockets          -< (re, spawnRs)
    scaleP          <- scaleA           -< gi
  returnA -< scaleP $ Pictures $ (drawRectangle white <$> fst <$> (
    elems rs ++
    [p]
    )) ++
    (draw <$> elems as)
    -- [color white $ text $ show $ sum $ score <$> elems kills]

drawRectangle :: G.Color -> [V2 Double] -> Picture
drawRectangle c = color c . polygon . fmap (\(V2 x y) -> (double2Float x, double2Float y))

defaultPlay :: SF (Event InputEvent) Picture -> IO ()
defaultPlay = playYampa (InWindow "Space Invaders" (1000, 600) (200, 200)) black 60

main :: IO ()
main = defaultPlay $ input >>> game'
