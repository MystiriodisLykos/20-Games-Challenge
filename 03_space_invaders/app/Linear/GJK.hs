module Linear.GJK ( collision'
                  , minkCircle
                  , minkRectangle, minkRectangle'
                  , minkPoly
                  , minkSegment) where

import Data.Maybe (fromMaybe)

import GJK.Collision (collision)
import GJK.Mink      (Mink)
import GJK.Point     (Pt, dot)
import GJK.Support   (polySupport)

import Linear.V2 ( V2 (V2) )

collision' :: (Mink a, Mink b) -> Bool
collision' = (fromMaybe False) . uncurry (collision 10)

circleSupport :: (Double, V2 Double) -> Pt -> Maybe Pt
circleSupport (r, (V2 x y)) d@(a,b) =
  let
    len = sqrt $ dot d d
  in Just ((a*r/len+x),(b*r/len+y))


minkCircle :: Double -> V2 Double -> Mink (Double, V2 Double)
minkCircle r p = ((r, p), circleSupport)

polySupport' :: [V2 Double] -> Pt -> Maybe Pt
polySupport' = polySupport . (fmap (\(V2 a b) -> (a, b)))

minkPoly :: [V2 Double] -> Mink [V2 Double]
minkPoly points = (points, polySupport')

minkRectangle :: V2 Double -> V2 Double -> Mink [V2 Double]
minkRectangle c s@(V2 w h) = minkPoly points
  where points = ((+) $ V2 (-w/2) (-h/2)) <$> [c, c + (V2 w 0), c + s, c + (V2 0 h)]

-- flip size and position arguments
minkRectangle' :: V2 Double -> V2 Double -> Mink [V2 Double]
minkRectangle' s p = minkRectangle p s

minkSegment :: V2 Double -> V2 Double -> Mink (V2 Double, V2 Double)
minkSegment a b = ((a, b), support)
  where
    support (a', b') = polySupport' [a', b']
