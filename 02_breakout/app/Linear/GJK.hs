module Linear.GJK (minkCircle, minkRectangle, minkPoly) where

import GJK.Mink      (Mink(..))
import GJK.Point     (Pt, dot)
import GHC.Float (float2Double)
import Debug.Trace (trace)

import Linear.V2 ( V2 (V2) )

circleSupport :: (Double, V2 Double) -> Pt -> Maybe Pt
circleSupport (r, (V2 x y)) d@(a,b) =
  let
    len = sqrt $ dot d d
  in Just ((a*r/len+x),(b*r/len+y))


polySupport :: [V2 Double] -> Pt -> Maybe Pt
polySupport list d =
    let
        dotList = map (dot d . pt) list
        decorated = zipWith (,) dotList list
        max' = foldl1 (\a@(a_d, _) b@(b_d, _)-> if a_d>=b_d then a else b) decorated
    in
        case max' of
          (m, p) -> Just $ pt p
    where
      pt (V2 x y) = (x, y)

minkCircle :: Double -> V2 Double -> Mink (Double, V2 Double)
minkCircle r p = ((r, p), circleSupport)

minkPoly :: [V2 Float] -> Mink [V2 Double]
minkPoly points = ((fmap float2Double) <$> points, polySupport)

minkRectangle :: V2 Float -> V2 Float -> Mink [V2 Double]
minkRectangle c s = minkPoly $ points c s
  where
    points :: V2 Float -> V2 Float -> [V2 Float]
    -- points c s | (trace (show c ++ " " ++ show s) False) = undefined
    points c@(V2 x y) s@(V2 w h) = ((+) $ V2 (-w/2) (-h/2)) <$> [c, c + (V2 w 0), c + s, c + (V2 0 h)]
