module Linear.GJK (minkCircle, minkRectangle, minkPoly) where

import GJK.Mink      (Mink(..))
import GJK.Point     (Pt, dot)
import GJK.Support   (polySupport)
import GHC.Float (float2Double)
import Debug.Trace (trace)

import Linear.V2 ( V2 (V2) )

circleSupport :: (Double, V2 Double) -> Pt -> Maybe Pt
circleSupport (r, (V2 x y)) d@(a,b) =
  let
    len = sqrt $ dot d d
  in Just ((a*r/len+x),(b*r/len+y))


minkCircle :: Double -> V2 Double -> Mink (Double, V2 Double)
minkCircle r p = ((r, p), circleSupport)

minkPoly :: [V2 Double] -> Mink [V2 Double]
minkPoly points = (points, support)
  where
    support = polySupport . (fmap (\(V2 a b) -> (a, b)))

minkRectangle :: V2 Double -> V2 Double -> Mink [V2 Double]
minkRectangle c s = minkPoly $ points c s
  where
    points :: V2 Double -> V2 Double -> [V2 Double]
    -- points c s | (trace (show c ++ " " ++ show s) False) = undefined
    points c@(V2 x y) s@(V2 w h) = ((+) $ V2 (-w/2) (-h/2)) <$> [c, c + (V2 w 0), c + s, c + (V2 0 h)]
