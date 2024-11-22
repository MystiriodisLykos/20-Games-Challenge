module Linear.GJK ( minkCircle
                  , Center, Size
                  , Rectangle
                  , minkRectangle, rectanglePoints
                  , minkPoly
                  , minkSegment) where

import Data.Maybe (fromMaybe)

import GJK.Mink      (Mink)
import GJK.Point     (Pt, dot, add)
import GJK.Support   (polySupport)

import Linear.V2 ( V2 (V2) )

type Center = V2 Double
type Size = V2 Double
type Rectangle = (Center, Size)

len a = sqrt $ dot a a

-- rectangleSupport' :: Size -> Pt -> Maybe Pt
-- rectangleSupport' (V2 w h) (i, j)
--   | i == 0 = Just (0, sh)
--   | j == 0 = Just (sw, 0)
--   | i == 0 && j == 0 = Nothing
--   | otherwise =
--     let
--       i' = abs i
--       j' = abs j
--       p1 = (sw, w'/i' * j)
--       p2 = (h'/j' * i, sh)
--     in if len p1 < len p2 then Just p1 else Just p2
--   where
--     w' = w/2
--     h' = h/2
--     sw = w' * signum i
--     sh = h' * signum j

rectangleSupport' :: Size -> Pt -> Maybe Pt
rectangleSupport' (V2 w h) (i, j)
  | i == 0 && j == 0 = Nothing
  | i >= 0 && j >= 0 = Just ( w',  h')
  | i <  0 && j >  0 = Just (-w',  h')
  | i <= 0 && j <= 0 = Just (-w', -h')
  | i >  0 && j <  0 = Just ( w', -h')
  | otherwise        = undefined
  where
    w' = w/2
    h' = w/2

rectangleSupport :: Rectangle -> Pt -> Maybe Pt
rectangleSupport ((V2 x y), s) p = (add (x, y)) <$> rectangleSupport' s p

rectanglePoints :: Rectangle -> [V2 Double]
rectanglePoints (c, s@(V2 w h)) = ((+) $ V2 (-w/2) (-h/2)) <$> [c, c + (V2 w 0), c + s, c + (V2 0 h)]

circleSupport :: (Double, V2 Double) -> Pt -> Maybe Pt
circleSupport (r, (V2 x y)) d@(a,b) =
  let
    s = r/(len d)
  in Just (a*s+x, b*s+y)
  --   len = sqrt $ dot d d
  -- in Just ((a*r/len+x),(b*r/len+y))


minkCircle :: Double -> V2 Double -> Mink (Double, V2 Double)
minkCircle r p = ((r, p), circleSupport)

polySupport' :: [V2 Double] -> Pt -> Maybe Pt
polySupport' = polySupport . (fmap (\(V2 a b) -> (a, b)))

minkPoly :: [V2 Double] -> Mink [V2 Double]
minkPoly points = (points, polySupport')

minkRectangle :: Rectangle -> Mink Rectangle
minkRectangle r = (r, rectangleSupport)

-- minkRectangle :: V2 Double -> V2 Double -> Mink [V2 Double]
-- minkRectangle c s@(V2 w h) = minkPoly points
--   where points = ((+) $ V2 (-w/2) (-h/2)) <$> [c, c + (V2 w 0), c + s, c + (V2 0 h)]

-- -- flip size and position arguments
-- minkRectangle' :: V2 Double -> V2 Double -> Mink [V2 Double]
-- minkRectangle' s p = minkRectangle p s

minkSegment :: V2 Double -> V2 Double -> Mink (V2 Double, V2 Double)
minkSegment a b = ((a, b), support)
  where
    support (a', b') = polySupport' [a', b']
