{-# LANGUAGE Arrows #-}

import Control.Concurrent
import Control.Arrow                      ( returnA, (>>>), arr )
import FRP.Yampa                          ( SF, Event (Event, NoEvent)
                                          , VectorSpace(zeroVector, (*^), (^+^), dot)
                                          , integral, reactimate, now, notYet
                                          , constant, event, tag, hold, after
                                          , mapFilterE, isEvent, edge, lMerge
                                          , identity, accumHold, repeatedly, delayEvent
                                          , dpSwitch, never, edgeTag, mergeBy, switch)
import Graphics.Gloss                     ( Display (InWindow)
                                          , Color
                                          , Picture (Color,Translate, Pictures)
                                          , circleSolid, black, white, red )
import Debug.Trace ( trace )
import Linear.V2   ( V2 (V2), perp, ex, ey )
import qualified Linear.Vector as Vector
import qualified Linear.Metric as Metric
import Graphics.Gloss.Interface.FRP.Yampa ( InputEvent, playYampa )

instance (RealFloat a) => VectorSpace (V2 a) a where
  zeroVector = Vector.zero
  (*^) s = fmap ((*) s)
  (^+^) = (Vector.^+^)
  dot = Metric.dot

data BallState = BallState {
  bP :: V2 Float,
  bV :: V2 Float,
  bColor :: Color
} deriving (Eq, Show)

data Collision = VerticalCollision | HorizontalCollision | BothCollision deriving (Eq, Show)

vc :: Collision
vc = VerticalCollision

hc :: Collision
hc = HorizontalCollision

both :: Collision
both = BothCollision

data BallInput = BallInput {
  collision :: Event Collision,
  score :: Event ()
} deriving (Eq, Show)

reflect :: Collision -> V2 Float -> V2 Float
reflect VerticalCollision = (*) $ V2 1 (-1)
reflect HorizontalCollision = (*) $ V2 (-1) 1
reflect _ = (*) (-1)

pongBall :: BallState -> SF BallInput BallState
pongBall initial = switch ball pongBall
  where
    ball = proc bi -> do
      v <- accumHold $ bV initial -< reflect <$> collision bi
      p <- integral -< v
      s <- notYet -< score bi
      let bs = initial {bP = p + bP initial}
      returnA -< (bs, s `tag` initial {bV = v})

pongBallCollision :: SF BallState (Event Collision)
pongBallCollision = proc bs -> do
  vc <- edgeTag vc -< abs (y $ bP bs) >= 200
  hc <- edgeTag hc -< abs (x $ bP bs) >= 200
  let c = mergeBy col vc hc
  returnA -< c
  where
    y (V2 _ y) = y
    x (V2 x _) = x
    col _ _ = both

ball :: BallState -> SF BallInput BallState
ball initial = proc bi -> do
  rec
    bs <- pongBall initial -< bi'
    c <- pongBallCollision -< bs
    s <- repeatedly 5 () -< bi
    let bi' = BallInput c s
  returnA -< bs

drawBall :: BallState -> Picture
drawBall (BallState (V2 x y)  _ color) = Color color $ Translate x y $ circleSolid 10

-- ballTest :: [SF BallInput BallState] -> SF [BallState] [BallState]
-- ballTest objs = dpSwitch routeTest objs never undefined

-- routeTest :: [BallState] -> [sf] -> [(BallInput, sf)]
-- routeTest objs sfs = fmap ((,) BallInput) sfs
--   where
--     collisions = 

animateBall :: SF a Picture
animateBall = proc i -> do
  bs <- ball (BallState (V2 0 0) (V2 100 50) black) -< (BallInput NoEvent NoEvent)
  returnA -< drawBall bs

defaultPlay :: SF (Event InputEvent) Picture -> IO ()
defaultPlay = playYampa (InWindow "YampaDemo" (1280, 1050) (200, 200)) white 30

main :: IO ()
main = defaultPlay $ animateBall
--main = reactimate (return $ Event (G.EventKey (G.SpecialKey G.KeyLeft) G.Down (G.Modifiers G.Up G.Up G.Up) (1,1)))
--                  (\_ -> threadDelay 10000 >> return (0.1, Just (Event $ G.EventKey (G.SpecialKey G.KeyUp) G.Down (G.Modifiers G.Up G.Up G.Up) (1,1))))
--                  (\_ -> return (0.1,  Nothing))
--                  (\_ e -> putStrLn (show e) >> return False)
--                  (parseInput)
--                  (\_ (pos, vel) -> putStrLn ("pos: " ++ show pos ++ " vel: " ++ show vel) >> return False)
--                  (fallingBall 10)
