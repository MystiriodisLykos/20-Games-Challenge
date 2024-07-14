{-# LANGUAGE Arrows #-}

import Control.Arrow                      ( Arrow, returnA, (>>>), (^>>), (>>^), (&&&), arr, first )
import Control.Applicative                ( liftA2 )
import FRP.Yampa                          ( SF, Event (Event, NoEvent), VectorSpace((*^))
                                          , tag, catEvents, accumHold, constant, isEvent
                                          , accumHoldBy, edgeTag, repeatedly, gate, tagWith
                                          , edge, iPre, merge, integral, hold, dpSwitch, iPre
                                          , drSwitch, edgeJust, parB, pSwitchB, event, delay, dropEvents )
import Graphics.Gloss                     ( Display (InWindow)
                                          , Picture (Pictures, Translate, Color)
                                          , Color
                                          , circleSolid, polygon
                                          , white
                                          , black
                                          , text
                                          )
import Graphics.Gloss.Interface.FRP.Yampa ( InputEvent, playYampa )
import Linear.V2 (V2 (V2), perp)
import Linear.Metric (dot, norm)
import Linear.Vector ((^/))
import GJK.Collision (collision)
import GJK.Mink (Mink)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid (Sum)
import Data.List (elemIndices)
import Data.Bool (bool)
import GHC.Float (double2Float)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Debug.Trace (trace)

import Linear.GJK (minkCircle, minkRectangle, minkSegment)
import Linear.VectorSpace ()

type Pos = V2 Double
type Vel = V2 Double
type Size = V2 Double

type BounceV = Vel -> Vel
type BounceE = Event BounceV

type CircleMink = Mink (Double, Pos)
type BallMink = CircleMink
type RectangleMink = Mink [V2 Double]
type PaddleMink = RectangleMink
type BrickMink = RectangleMink

type ScreenSize = V2 Int

data VelDirection = VelForward | VelZero | VelBackward deriving (Show, Eq)

data GameInput = GameInput {
  keyLeft :: G.KeyState,
  keyRight :: G.KeyState,
  screenSize :: V2 Int
} deriving Show

-- Doesn't always work as expected when the surface
-- vector is parallel or orthogonal to the vector
-- in that case bounce twice
-- TODO: can I change this function to do 2 bounces?
bounce :: V2 Double -> BounceV
bounce s a = (norm a / norm b) *^ b
  where
    b = a - (2 * (dot a s) *^ s)

mergeC :: [Event (a -> a)] -> Event (a -> a)
mergeC = (fmap $ foldl1 (.)) . catEvents

vertical :: BounceV
vertical = bounce $ V2 0 1

horizontal :: BounceV
horizontal = bounce $ V2 1 0

position :: Pos -> SF Vel Pos
position p0 = integral >>^ ((+) p0)

bVelocity :: Vel -> SF BounceE Vel
bVelocity v0 = accumHold v0

lVelocity :: Vel -> SF VelDirection Vel
lVelocity v = arr (\d -> (d' d) * v)
  where
    d' VelForward = 1
    d' VelBackward = -1
    d' _ = 0

eventToMaybe :: Event a -> Maybe a
eventToMaybe = event Nothing Just

filterByList (True:bs)  (x:xs) = x : filterByList bs xs
filterByList (False:bs) (_:xs) =     filterByList bs xs
filterByList _          _      = []

collision' :: (Mink a, Mink b) -> Bool
collision' = (fromMaybe False) . uncurry (collision 10)

collisionCircle :: Double -> SF Pos BallMink
collisionCircle r = arr $ minkCircle r

-- flip size and position arguments
minkRectangle' :: V2 Double -> V2 Double -> Mink [V2 Double]
minkRectangle' s p = minkRectangle p s

collisionRectangle :: V2 Double -> SF Pos (Mink [V2 Double])
collisionRectangle s = arr $ minkRectangle' s

drawBall :: V2 Double -> Double -> Picture
drawBall (V2 x y) = Translate (double2Float x) (double2Float y) . circleSolid . double2Float

drawRectangle :: [V2 Double] -> Picture
drawRectangle = polygon . fmap (\(V2 x y) -> (double2Float x, double2Float y))

wallBounce :: SF (CircleMink, ScreenSize) BounceE
wallBounce = proc (((r, (V2 x y)), _) , (V2 w h)) -> do
  t <- edgeTag vertical -< y + r >= (int h)/2
  l <- edgeTag horizontal -< x + r >= (int w)/2
  r <- edgeTag horizontal -< (-x) + r >= (int w)/2
  returnA -< mergeC [t, l, r]
  where
    int = fromIntegral

paddleBounce :: SF (BallMink, PaddleMink) BounceE
paddleBounce = collision' ^>> edgeTag vertical

brickCollision :: SF (BallMink, BrickMink) Bool
brickCollision = arr collision'

-- fLift :: (Arrow a, Functor f) => a b c -> a (f b) (f c)
-- fLift a = proc bs -> do
--   returnA -< a <$ bs

brickCollisions :: SF (BallMink, [BrickMink]) [Event BrickMink]
brickCollisions = proc (b, bs) -> do
  returnA -< (\k -> (Event k) `gate` collision' (b, k) ) <$> bs

brickBounces :: SF (BallMink, [Event BrickMink]) BounceE
brickBounces = proc (b, bs) -> do
  -- TODO: use catEvents
  returnA -< mergeC $ Event <$> collision'' b <$> (catMaybes $ eventToMaybe <$> bs)
  where
    -- TODO: break this out
    collision'' :: BallMink -> BrickMink -> BounceV
    collision'' b (bp, _) = foldl (.) id $ bounces <$> (collisions $ segment <$> edges)
      where
        edges = zip bp (drop 1 $ cycle bp)
        segment = uncurry minkSegment -- . (\es -> trace (show es) es)
        collisions = filter (\a -> collision' (a, b))
        bounces ((a, b), _) = let v = (a-b) in bounce $ perp $ v ^/ norm v

ballReset :: SF (BallMink, ScreenSize) (Event ())
ballReset = proc (((r, (V2 x y)), _) , (V2 w h)) -> edge -< y < -(fromIntegral h)/2

acc :: SF Vel Vel
acc = proc v -> do
  a <- integral -< 0.05
  returnA -< (1 + a) * v

ball :: SF BounceE BallMink
ball = (bVelocity $ V2 50 (-100)) >>> acc >>> (position $ V2 0 (-50)) >>> (collisionCircle 8)

paddle :: SF VelDirection PaddleMink
paddle = lVelocity (V2 100 0) >>> (position $ V2 0 (-100)) >>> (collisionRectangle $ V2 50 5)

-- brick :: Pos -> SF BallMink (Maybe BrickMink)
-- brick p = proc b -> do
--   bm <- collisionRectangle $ V2 50 20 -< p
--   c <- edge -< collision' (bm, b)
--   c' <- hold Nothing -< Event $ event (Just bm) (const Nothing) c
--   returnA -< c'

brick :: Size -> Pos -> SF (Event ()) (Maybe BrickMink)
brick s p = proc e -> do
  bm <- collisionRectangle s -< p
  c <- hold Just -< e `tag` (const Nothing)
  returnA -< c bm

-- arrList :: SF a b -> SF [a] [b]
-- arrList sf = 

-- cat' :: SF a b -> SF a b -> SF [a] [b]
-- cat' a b = 

-- flat' :: [SF a b] -> SF [a] [b]
-- flat' sfs = undefined

bricks :: [SF (Event ()) (Maybe BrickMink)] -> SF [Event a] [BrickMink]
bricks bs0 = bricks' bs0 >>^ catMaybes
  where
    bricks' bs = dpSwitch route bs kill cont
    route es = zip $ (fmap (tagWith ()) es) ++ repeat NoEvent
    kill = (coll . (fmap (not . null)) . snd) ^>> dropEvents 1
    coll bs = bool (Event bs) NoEvent (and bs)
    -- cont _ ds | (trace (show ds) False) = undefined
    cont sfs ds = bricks' $ filterByList ds sfs

bricks1 = bricks [brickN (if y<3 then 1 else 2) (V2 50 20) (V2 (x*60) (y*30+5)) | x <- [-2..2], y <- [1..3]]

brickN :: Int -> Size -> Pos -> SF (Event ()) (Maybe BrickMink)
brickN n s p = (countDown n) >>> (brick s p)

countDown :: Int -> SF (Event ()) (Event ())
countDown n = proc e -> do
  i <- accumHold n -< e `tag` ((+) (-1))
  e <- edge -< i <= 0
  returnA -< e

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
    r               <- ballReset       -< (b, screenSize gi)
    bcs             <- brickCollisions -< (b, bs)
    dbcs            <- iPre []         -< bcs
    wb              <- wallBounce      -< (b, screenSize gi)
    pb              <- paddleBounce    -< (b, p)
    bb              <- brickBounces    -< (b, bcs)
    p@(ps, _)       <- paddle          -< paddleD (keyRight gi) (keyLeft gi)
    b@((br, bp), _) <- drSwitch ball   -< (mergeC [wb, pb, bb], r `tag` ball)
    bs              <- bricks1         -< dbcs
  returnA -< Pictures $ [ drawBall bp br
                        , drawRectangle ps
                        ] ++ (drawRectangle <$> fst <$> bs)

defaultPlay :: SF (Event InputEvent) Picture -> IO ()
defaultPlay = playYampa (InWindow "Breakout" (300, 500) (200, 200)) white 60

main :: IO ()
main = defaultPlay $ input >>> game'
