{-# LANGUAGE Arrows, KindSignatures, MultiParamTypeClasses, ConstraintKinds, FlexibleInstances, FlexibleContexts, AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables #-}

import Control.Arrow                      ( returnA, (>>>), (^>>), (>>^), (&&&), (***), arr, first, second )
import Control.Applicative                ( Alternative, empty, (<|>) )
import FRP.Yampa                          ( SF, Event (Event, NoEvent), DTime, reactInit, react, time
                                          , tag, catEvents, accumHold, after, notYet, isEvent
                                          , accumHoldBy, dSwitch, constant, iPre, event
                                          , edge, integral, hold, switch, rSwitch
                                          , iEdge, repeatedly, switch, dropEvents )
import Graphics.Gloss                     ( Display (InWindow)
                                          , Picture (Pictures)
                                          , Color, blank
                                          , polygon, scale
                                          , white, black
                                          , green, blue
                                          , red, yellow
                                          , color, text
                                          )
-- import Graphics.Gloss.Interface.FRP.Yampa ( InputEvent, playYampa )
import Linear.V2 (V2 (V2))
import Linear.Vector (Additive, lerp, zero)
import GJK.Mink (Mink)
import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Either (isLeft)
import Data.Kind (Constraint)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import GHC.Float (double2Float, int2Float)
import qualified Graphics.Gloss.Interface.IO.Game as G

import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (exitSuccess)
import           Data.IORef                       (newIORef, readIORef,
                                                   writeIORef)
import           Control.Monad                    (when)

import Linear.GJK         ( minkRectangle, rectanglePoints, Rectangle )
import Linear.VectorSpace ()
import Data.DMap          (toMap, elems, fromList, partition, DMap (DMap), IMap)
import Data.Table.DMap    ()
import GJK.Game           ( WithCollision, mink
                          , CollisionsC
                          , collisions
                          )
import FRP.Yampa.Game     ( WithKillFlag (..)
                          , switchAfter, onlyEvery
                          , pKillSpawn
                          , switchWhenE
                          , partAliveDeadE
                          , tagOnE
                          )

import Witherable as W

class WithScore a where
  score :: a -> Int

class Drawable a where
  draw :: a -> Picture

type AlienType a = ( WithKillFlag a
                   , WithCollision Rectangle a
                   , WithScore a
                   , Drawable a) :: Constraint

data BasicAlienType = RedAlien {aDead :: Bool, aPos :: Pos}
                | BlueAlien {aDead :: Bool, aPos :: Pos}
                | GreenAlien {aDead :: Bool, aPos :: Pos}
                | YellowAlien {aDead :: Bool, aPos :: Pos}
                deriving (Show, Eq)

basicAlien :: (Bool -> Pos -> a) -> BasicAlienType -> a
basicAlien f a = f (aDead a) (aPos a)

instance WithCollision Rectangle RectangleMink where
  mink = id

instance WithKillFlag (BasicAlienType) where
  killF = aDead

instance WithCollision Rectangle BasicAlienType where
  mink a = minkRectangle (aPos a, V2 30 30)

instance WithScore BasicAlienType where
  score (RedAlien _ _) = 1
  score (BlueAlien _ _) = 2
  score (GreenAlien _ _) = 3
  score (YellowAlien _ _) = 4

instance Drawable BasicAlienType where
  draw a = draw' a $ color' a
    where
      draw' a' c = drawRectangle c $ rectanglePoints $ fst $ mink a'
      -- color' a | (trace (show a) False) = undefined
      color' (RedAlien _ _) = red
      color' (BlueAlien _ _) = blue
      color' (GreenAlien _ _) = green
      color' (YellowAlien _ _) = yellow

type Pos = V2 Double
type Vel = V2 Double
type Size = V2 Double

type RectangleMink = Mink Rectangle
type PaddleMink = RectangleMink
type RocketMink = RectangleMink

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
aVelocity =
  -- constant (V2 0 0)
  lVel
  where
    switch' :: Double -> Vel -> SF a Vel -> SF a Vel
    switch' d v b = switchAfter d (constant v) b
    lVel   = switch' 10  (V2 (-50) 0) (dVel rVel)
    rVel   = switch' 10  (V2   50  0) (dVel lVel)
    dVel n = switch' 0.5 (V2 0 (-50)) n

aVelocity2 :: SF a Vel
aVelocity2 =
  lVel
  where
    switch' :: Double -> Vel -> SF a Vel -> SF a Vel
    switch' d v b = switchAfter d (constant v) b
    lVel   = switch' 18 (V2 (-50) 0) (dVel rVel)
    rVel   = switch' 18 (V2   50  0) (dVel lVel)
    dVel n = switch' 1  (V2 0 (-50)) n

collisionRectangle :: V2 Double -> SF Pos (Mink Rectangle)
collisionRectangle s = arr (\p -> minkRectangle (p, s))

ship :: Ship
ship = lVelocity (V2 100 0) >>> (position $ V2 0 (-200)) >>> (collisionRectangle $ V2 50 5)

rocket :: Pos -> Rocket ()
rocket p = switchWhenE $ constant (V2 0 50) >>> position p >>> collisionRectangle (V2 20 20)

rocket' :: Pos -> Rocket ()
rocket' p = fmap Left ^>> vBoundRocket 300 (rocket p)

-- This isn't working
vBoundRocket :: Double -> Rocket a -> Rocket (Either a Double)
vBoundRocket iTop r = proc e -> do
  r' <- r -< e >>= leftEvent
  top <- hold iTop -< e >>= rightEvent
  returnA -< r' >>= cap top
  where
    cap :: Double -> RocketMink -> Maybe RocketMink
    cap top' (rps, _) | (over top' $ fst rps) = Nothing
    cap _    rm                               = Just rm
    over :: Double -> Pos -> Bool
    over ym (V2 _ y) = y > ym

basicGun :: Gun Bool ()
basicGun = onlyEvery 1 $ second (iEdge False) >>^ (\(p, e) -> e `tag` [rocket' p, rocket' $ p + (V2 40 0)])

alienMovement :: Pos -> SF a Pos
alienMovement i = aVelocity >>> position i

redAlien :: Pos -> BasicAlien a
redAlien i = switch (tagOnE $
                          alienMovement i >>^
                          RedAlien False)
                  (\l -> constant l{aDead=True})

blueAlien :: Pos -> BasicAlien a
blueAlien i = redAlien i >>^ basicAlien BlueAlien

greenAlien :: Pos -> BasicAlien a
greenAlien i = redAlien i >>^ basicAlien GreenAlien

yellowAlien :: Pos -> BasicAlien a
yellowAlien i = redAlien i >>^ basicAlien YellowAlien

alienTypes = [redAlien, greenAlien, blueAlien, yellowAlien]

-- aliens1 = [c (V2 x y) | x      <- take 9 [50,100..]
--                       , (c, y) <- take 4 $ zip alienTypes [100,150..]]

aliens1 = [c (V2 x y) | x      <- take 17 [(-250),(-200)..]
                      , (c, y) <- take 7 $ zip (cycle alienTypes) [(-50),0..]]

-- pKillSpawnI i a = second (index $ 1 + length i) >>> pKillSpawn i (fromList i)

aliens :: AlienType a =>
  [SF (Event c) a] ->
  SF (IMap (Event c), Event [SF (Event c) a]) (IMap a, Event (IMap a))
aliens i = second (index $ 1 + length i)
           >>> pKillSpawn NoEvent (fromList i)
           >>> partAliveDeadE

newAlien :: Pos -> BasicAlien a
newAlien i = switch (tagOnE $ aVelocity2
                      >>> position i
                      >>^ RedAlien False)
                  (\l -> constant l{aDead=True})

aSpawn :: SF a (Event [BasicAlien b])
aSpawn = repeatedly 1 [newAlien $ V2 450 250]

rockets :: SF (IMap (Event a), Event [Rocket a]) (IMap RocketMink)
rockets = second (index 0) >>> pKillSpawn NoEvent empty >>^ W.catMaybes

index :: Int -> SF (Event [a]) (Event (IMap a))
index i0 = proc as -> do
  rec
    s <- iPre i0 -< e
    e <- accumHoldBy (\p n -> (length n) + p) i0 -< as
  returnA -< (DMap Nothing . Map.fromAscList . zip [s..e]) <$> as

scaleA :: SF GameInput (Picture -> Picture)
scaleA = arr $ (\(V2 x y) -> scale x y) . scale' . size'
  where
    size' = (fmap int2Float) . screenSize
    scale' sizeC = sizeC / size' giI

parseGameInput :: GameInput -> InputEvent -> GameInput
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyLeft) s _ _)  = gi { keyLeft = s }
parseGameInput gi (G.EventKey (G.SpecialKey G.KeyRight) s _ _) = gi { keyRight = s }
parseGameInput gi (G.EventKey (G.SpecialKey G.KeySpace) s _ _) = gi { keyFire = s }
parseGameInput gi (G.EventResize (x, y)) = gi {screenSize = V2 x y}
parseGameInput gi _ = gi

giI = GameInput G.Up G.Up G.Up (V2 1000 600)

input :: SF (Event InputEvent) GameInput
input = accumHoldBy parseGameInput giI

-- exit after 10 seconds, useful for profiling
-- input :: SF (Event InputEvent) GameInput
-- input = proc e -> do
--   gi <- accumHoldBy parseGameInput giI -< e
--   e <- after 10 (unsafePerformIO exitSuccess) -< ()
--   returnA -< event gi id e

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

polyCollisions :: (CollisionsC Rectangle Rectangle a b f g
                  , Alternative f
                  , Alternative g)
  => SF (f a, g b) (f (Event ()), g (Event ()))
polyCollisions = arr collisions' >>> (iPre (empty, empty))
  where
    collisions' = bimap e e . (uncurry $ collisions @Rectangle @Rectangle)
    e f = Event () <$ f

game' :: SF GameInput Picture
game' = proc gi -> do
  rec
    p@((ps, _), _)  <- ship             -< shipD (keyRight gi) (keyLeft gi)
    spawnRs         <- basicGun         -< (ps, keyFire gi == G.Down)
    -- ae              <- collisionTest () -< as
    -- asp             <- aSpawn           -< ()
    (as, kills)     <- aliens aliens1   -< (ae, NoEvent)
    -- re              <- collisionTest () -< rs
    rs              <- rockets          -< (re, spawnRs)
    (re, ae)        <- polyCollisions   -< (rs, as)
    scaleP          <- scaleA           -< gi
  returnA -< scaleP $ Pictures $
    (drawRectangle white <$>
      rectanglePoints . fst <$> (
        -- elems (trace (show $ length rs) rs) ++
        elems rs ++
        [p])
    ) ++
    (draw <$> elems as)
    -- [color white $ text $ show $ sum $ score <$> elems kills]

drawRectangle :: G.Color -> [V2 Double] -> Picture
drawRectangle c = color c . polygon . fmap (\(V2 x y) -> (double2Float x, double2Float y))

defaultPlay :: SF GameInput Picture -> IO ()
defaultPlay = playYampa' (InWindow "Space Invaders" (1000, 600) (200, 200)) black 60 input giI

main :: IO ()
main = defaultPlay game'

type InputEvent = G.Event


-- | Play the game in a window, updating when the value of the provided
playYampa' :: Display                 -- ^ The display method
           -> Color                   -- ^ The background color
           -> Int                     -- ^ The refresh rate, in Hertz
           -> SF (Event InputEvent) a -- ^ Handle inputs
           -> a                       -- ^ initial input
           -> SF a Picture            -- ^ Game function
           -> IO ()
playYampa' display color frequency input i mainSF = do
  picRef <- newIORef blank
  inputRef <- newIORef i

  handle <- reactInit
              (return i)
              (\_ updated pic -> do when updated (picRef `writeIORef` pic)
                                    return False
              )
              mainSF

  inputHandle <- reactInit
                   (return NoEvent)
                   (\_ updated i' -> do when updated (inputRef `writeIORef` i')
                                        return False
                   )
                   input

  let -- An action to convert the world to a picture
      toPic :: DTime -> IO Picture
      toPic = const $ readIORef picRef

      -- A function to handle input events
      handleInput :: G.Event -> DTime -> IO DTime
      handleInput event timeAcc = do
        _quit <- react inputHandle (delta, Just $ Event event)
        return (timeAcc + delta)
        -- case event of
        -- The issue with lag with the mouse moves is that a mouse movement
        -- causes the whole scrren to redraw, even though we don't use that input
        -- this just ignores motion events and fixes the problem
            -- (G.EventMotion _) -> return (timeAcc + delta)
            -- otherwise -> do
            --     _quit <- react handle (delta, Just (Event (trace (show event) event)))
            --     return (timeAcc + delta)
        where
          delta = 0.01 / fromIntegral frequency

      -- A function to step the world one iteration. It is passed the period of
      -- time (in seconds) needing to be advanced
      stepWorld :: Float -> DTime -> IO DTime
      stepWorld delta timeAcc
          | delta' > 0 = do
              i' <- readIORef inputRef
              _ <- react handle (delta', Just i')
              return 0.0
          | otherwise  = return (-delta')
        where
          delta' = realToFrac delta - timeAcc

  G.playIO display color frequency 0 toPic handleInput stepWorld
