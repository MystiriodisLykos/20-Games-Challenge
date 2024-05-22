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
  reset :: Event ()
} deriving (Eq, Show)

type CollisionSF a = SF a (Event Collision)
type Score a = SF a (Event ())

v2x :: V2 a -> a
v2x (V2 x _) = x

v2y :: V2 a -> a
v2y (V2 _ y) = y

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
      s <- notYet -< reset bi
      let bs = initial {bP = p + bP initial}
      returnA -< (bs, s `tag` initial {bV = v})

exampleBallCollision :: CollisionSF (a, BallState)
exampleBallCollision = proc (_, bs) -> do
  vc <- edgeTag vc -< abs (v2y $ bP bs) >= 200
  hc <- edgeTag hc -< abs (v2x $ bP bs) >= 200
  let c = mergeBy (const $ const both) vc hc
  returnA -< c

exampleBallScore :: Score a
exampleBallScore = repeatedly 7 ()

ball :: BallState -> CollisionSF (a, BallState) -> Score (a, BallState) -> SF a BallState
ball initial c s = proc bi -> do
  rec
    c <- c -< (bi, bs)
    s <- s -< (bi, bs)
    bs <- pongBall initial -< BallInput c s
  returnA -< bs

drawBall :: BallState -> Picture
drawBall (BallState (V2 x y)  _ color) = Color color $ Translate x y $ circleSolid 10

exampleBall :: SF a BallState
exampleBall = ball (BallState (V2 0 0) (V2 100 (-50)) black) exampleBallCollision exampleBallScore

animateBall :: SF a BallState ->  SF a Picture
animateBall ball = proc i -> do
  bs <- ball -< i
  returnA -< drawBall bs

defaultPlay :: SF (Event InputEvent) Picture -> IO ()
defaultPlay = playYampa (InWindow "YampaDemo" (1280, 1050) (200, 200)) white 30

main :: IO ()
main = defaultPlay $ animateBall exampleBall
--main = reactimate (return $ Event (G.EventKey (G.SpecialKey G.KeyLeft) G.Down (G.Modifiers G.Up G.Up G.Up) (1,1)))
--                  (\_ -> threadDelay 10000 >> return (0.1, Just (Event $ G.EventKey (G.SpecialKey G.KeyUp) G.Down (G.Modifiers G.Up G.Up G.Up) (1,1))))
--                  (\_ -> return (0.1,  Nothing))
--                  (\_ e -> putStrLn (show e) >> return False)
--                  (parseInput)
--                  (\_ (pos, vel) -> putStrLn ("pos: " ++ show pos ++ " vel: " ++ show vel) >> return False)
--                  (fallingBall 10)