{-# LANGUAGE Arrows #-}

import Control.Arrow                      ( returnA, (>>>), (^>>), (>>^), (&&&), (***), arr, first, second )
import Control.Applicative                ( Alternative, empty, (<|>) )
import FRP.Yampa                          ( SF, Event (Event, NoEvent)
                                          , tag, catEvents, accumHold, after, notYet, isEvent
                                          , accumHoldBy, dSwitch, constant, iPre, event
                                          , edge, integral, hold, switch, rSwitch
                                          , iEdge, repeatedly, switch, dropEvents )
import FRP.Yampa.AltSwitches              ( drpSwitchA, safePair )
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
import Data.Either (isLeft)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import GHC.Float (double2Float, int2Float)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Debug.Trace (trace)

import Linear.GJK (minkRectangle)
import Linear.VectorSpace ()

import Witherable as W

data DMap k v = DMap (Maybe v) (Map.Map k v) deriving Show

type IMap v = DMap Int v

toMap :: DMap k v -> Map.Map k v
toMap (DMap _ m) = m

elems :: DMap k v -> [v]
elems (DMap _ m) = Map.elems m

fromList :: [v] -> IMap v
fromList as = DMap Nothing $ Map.fromAscList $ zip [0..] as

instance Functor (DMap k) where
  fmap f (DMap d m) = DMap (f <$> d) (fmap f m)

instance (Ord k) => Applicative (DMap k) where
  pure x = DMap (Just x) Map.empty

  (<*>) :: DMap k (a -> b) -> DMap k a -> DMap k b
  DMap df fs <*> DMap dx xs = DMap (df <*> dx) $ Map.unions
    [ Map.intersectionWith ($) fs xs, fFallback, xFallback ]
    where
      fFallback =
        case df of
          Nothing -> mempty
          Just f  -> fmap f xs

      xFallback =
        case dx of
          Nothing -> mempty
          Just x  -> fmap ($ x) fs

instance (Ord k) => Alternative (DMap k) where
  empty = DMap Nothing Map.empty

  DMap dl ml <|> DMap dr mr = DMap (dl <|> dr) (Map.union ml mr)

instance W.Filterable (DMap k) where
  mapMaybe f (DMap d m) = DMap (d >>= f) (mapMaybe f m)

instance Foldable (DMap k) where
  foldMap f (DMap d m) = maybe mempty f d <> foldMap f m

class WithKillFlag a where
  killF :: a -> Bool

instance (Foldable f) => WithKillFlag (f a) where
  killF = null

newtype AlienOut = AlienOut {unAlienOut :: Either AlienMink (Event Int)}

instance WithKillFlag AlienOut where
  killF (AlienOut (Left _))  = False
  killF (AlienOut (Right _)) = True

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
type Alien a = SF (Event a) (AlienOut)

filterBy :: (Alternative col, W.Filterable col) => col Bool -> col a -> col a
filterBy bs as = W.mapMaybe map' $ safePair True bs as
  where
    map' (True, a) = Just a
    map' _          = Nothing

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
arrUntil sf = switch (first $ sf >>^ Just) (const $ constant Nothing)

arrUntilEvent :: SF () b -> SF (Event a) (Maybe b)
arrUntilEvent sf = ((,) ()) ^>> arrUntil sf

switchAfter :: Double -> SF a b -> SF a b -> SF a b
switchAfter n a b = switch (a &&& after n b) id

onlyEveryT :: Double -> SF a (Event b) -> SF a (Event b)
onlyEveryT t sf = dSwitch (sf >>^ (\e -> (e, e `tag` ()))) cont
  where
    cont _ = switch (after t () >>^ (\e -> (NoEvent, e))) (const $ onlyEveryT t sf)

drpKillSwitchZ :: (WithKillFlag b, Alternative col, W.Filterable col, Foldable col)
  => a
  -> col (SF a b)
  -> SF (col a, Event (col (SF a b) -> col (SF a b))) (col b)
drpKillSwitchZ a sfs = proc (as, e) -> do
  rec
    bs <- drpSwitchA a sfs -< (as, mergeC [killE bs, e])
  returnA -< bs
  where
    killE bs = event' $ (not . killF) <$> bs
    -- event' bs | (trace (show $ and bs) False) = undefined
    event' bs = bool (Event $ filterBy bs) NoEvent (and bs)

spawnC :: (WithKillFlag b, Alternative col, W.Filterable col, Foldable col)
  => SF (Event (col (SF a b))) (Event (col (SF a b) -> col (SF a b)))
spawnC = arr $ fmap $ flip (<|>)

pKillSpawnZ :: (WithKillFlag b, Alternative col, W.Filterable col, Foldable col)
  => a
  -> col (SF a b)
  -> SF (col a, Event (col (SF a b))) (col b)
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

basicAlien p s = ((,) ()) ^>> switch
  (first $ aVelocity >>> position p >>> collisionRectangle (V2 30 30) >>^ (AlienOut . Left))
  (const $ constant $ AlienOut $ Right $ Event s)

aliens1 = fromList [basicAlien (V2 x y) 10 | x <- [50,100..450], y <- [100,150..250]]

aliens :: SF (IMap (Event a), Event [Alien a]) (IMap AlienMink, Event Int)
aliens = second index
  >>> pKillSpawnZ NoEvent aliens1
  >>^ Map.mapEither id . (fmap unAlienOut) . toMap
  >>> (arr $ DMap Nothing) *** (arr $ fmap sum . catEvents . Map.elems)

rockets :: SF (IMap (Event a), Event [Rocket a]) (IMap RocketMink)
rockets = second index >>> pKillSpawnZ NoEvent empty >>^ W.catMaybes

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
    (as, kills)     <- aliens           -< (ae, NoEvent)
    re              <- collisionTest () -< rs
    rs              <- rockets          -< (re, spawnRs)
    scaleP          <- scaleA           -< gi
  returnA -< scaleP $ Pictures $ drawRectangle <$> fst <$> (
    elems rs ++
    elems as ++
    [p]
    )

drawRectangle :: [V2 Double] -> Picture
drawRectangle = color white . polygon . fmap (\(V2 x y) -> (double2Float x, double2Float y))

defaultPlay :: SF (Event InputEvent) Picture -> IO ()
defaultPlay = playYampa (InWindow "Space Invaders" (1000, 600) (200, 200)) black 60

main :: IO ()
main = defaultPlay $ input >>> game'
