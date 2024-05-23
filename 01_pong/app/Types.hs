module Types (v2x, v2y) where

import Linear.V2   ( V2 (V2) )
import FRP.Yampa (VectorSpace(..))
import qualified Linear.Vector as Vector
import qualified Linear.Metric as Metric

instance (RealFloat a) => VectorSpace (V2 a) a where
  zeroVector = Vector.zero
  (*^) s = fmap ((*) s)
  (^+^) = (Vector.^+^)
  dot = Metric.dot

v2x :: V2 a -> a
v2x (V2 x _) = x

v2y :: V2 a -> a
v2y (V2 _ y) = y
