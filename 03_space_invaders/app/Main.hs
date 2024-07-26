{-# LANGUAGE Arrows #-}

import Control.Arrow                      ( returnA, (>>>), (^>>), (>>^), (&&&), arr, first, second )
import FRP.Yampa                          ( SF, Event (Event, NoEvent)
                                          , tag, catEvents, accumHold, after
                                          , accumHoldBy, dSwitch, constant, iPre
                                          , edge, integral, hold, switch, rSwitch
                                          , iEdge )
import FRP.Yampa.Switches                 ( drpSwitchZ, parB )
import Graphics.Gloss                     ( Display (InWindow)
                                          , Picture (Pictures)
                                          , polygon, scale
                                          , white, black, color
                                          )
import Graphics.Gloss.Interface.FRP.Yampa ( InputEvent, playYampa )
import Linear.V2 (V2 (V2))
import Linear.Vector (Additive, lerp, zero)
import GJK.Collision (collision)
import GJK.Mink (Mink)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Bool (bool)
import GHC.Float (double2Float, int2Float)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Debug.Trace (trace)

import Linear.GJK (minkRectangle)
import Linear.VectorSpace ()

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

type KSF a b = SF a (Maybe b)
type Rocket a = SF (Event a) (Maybe RocketMink)
type Gun a b = SF (Pos, a) (Event [Rocket b])
type Ship = SF VelDirection PaddleMink
type Alien a = SF (Event a) (Either AlienMink (Event Int))

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

avg :: (Fractional a, Additive f) => [f a] -> f a
avg []     = zero
avg [x]    = x
avg (x:xs) = lerp 0.5 x $ avg xs

mergeC :: [Event (a -> a)] -> Event (a -> a)
mergeC = (fmap $ foldl1 (.)) . catEvents

arrUntil :: SF a b -> SF (a, Event c) (Maybe b)
arrUntil sf = dSwitch (first $ sf >>^ Just) (const $ constant Nothing)

arrUntilEvent :: SF () b -> SF (Event a) (Maybe b)
arrUntilEvent sf = ((,) ()) ^>> arrUntil sf

switchAfter :: Double -> SF a b -> SF a b -> SF a b
switchAfter n a b = switch (a &&& after n b) id

onlyEveryT :: Double -> SF a (Event b) -> SF a (Event b)
onlyEveryT t sf = dSwitch (sf >>^ (\e -> (e, e `tag` ()))) cont
  where
    cont _ = switch (after t () >>^ (\e -> (NoEvent, e))) (const $ onlyEveryT t sf)

drpKillSwitchZ :: a -> [KSF a b] -> SF ([a], Event ([KSF a b] -> [KSF a b])) [b]
drpKillSwitchZ a sfs = proc (as, e) -> do
  rec
    bs <- drpSwitchZ sfs -< (as ++ repeat a, mergeC [kill, e])
    kill <- kill' -< bs
  returnA -< catMaybes bs
  where
    kill' = arr (coll . (fmap (not . null)))
    coll bs = bool (Event $ filterByList $ bs ++ repeat True) NoEvent (and bs)

spawnC :: SF (Event ([KSF a b])) (Event ([KSF a b] -> [KSF a b]))
spawnC = arr $ fmap $ flip (++)

pKillSpawnZ :: a -> [KSF a b] -> SF ([a], Event ([KSF a b])) [b]
pKillSpawnZ a sfs = second spawnC >>> drpKillSwitchZ a sfs

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
rocket p = arrUntilEvent $ constant (V2 0 50) >>> position p >>> collisionRectangle (V2 20 20)

rocket' :: Pos -> Rocket ()
rocket' p = fmap Left ^>> vBoundRocket 300 (rocket p)

vBoundRocket :: Double -> Rocket a -> Rocket (Either a Double)
vBoundRocket iTop r = proc e -> do
  r' <- r -< e >>= (either Event (const NoEvent))
  top <- hold iTop -< e >>= (either (const NoEvent) Event)
  returnA -< r' >>= cap top
  where
    cap :: Double -> RocketMink -> Maybe RocketMink
    cap top' (rps, _) | (or $ over top' <$> rps) = Nothing
    cap _    rm                                  = Just rm
    over :: Double -> Pos -> Bool
    over ym (V2 _ y) = y > ym

basicGun :: Gun Bool ()
basicGun = onlyEveryT 1 $ second (iEdge False) >>^ (\(p, e) -> e `tag` [rocket' p])

basicAlien p = aVelocity >>> position p >>> collisionRectangle (V2 30 30)

aliens = parB [basicAlien (V2 x y) | x <- [50,100..450], y <- [100,150..250]]

rockets :: SF ([Event a], Event [Rocket a]) [RocketMink]
rockets = pKillSpawnZ NoEvent []

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

game' :: SF GameInput Picture
game' = proc gi -> do
  rec
    p@(ps, _)       <- ship       -< shipD (keyRight gi) (keyLeft gi)
    spawnRs         <- basicGun   -< (avg ps, keyFire gi == G.Down)
    as              <- aliens     -< ()
    rs              <- rockets    -< ([NoEvent], spawnRs)
    scaleP          <- scaleA     -< gi
  returnA -< scaleP $ Pictures $ drawRectangle <$> fst <$> (rs ++ as ++ [p])

drawRectangle :: [V2 Double] -> Picture
drawRectangle = color white . polygon . fmap (\(V2 x y) -> (double2Float x, double2Float y))

defaultPlay :: SF (Event InputEvent) Picture -> IO ()
defaultPlay = playYampa (InWindow "Space Invaders" (1000, 600) (200, 200)) black 60

main :: IO ()
main = defaultPlay $ input >>> game'
