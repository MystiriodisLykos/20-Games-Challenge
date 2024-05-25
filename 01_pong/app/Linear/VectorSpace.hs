{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Linear.VectorSpace where

import Linear.V2 (V2)
import FRP.Yampa (VectorSpace(..))
import qualified Linear.Vector as Vector
import qualified Linear.Metric as Metric

instance RealFloat a => VectorSpace (V2 a) a where
  zeroVector = Vector.zero
  (*^) s = fmap ((*) s)
  (^+^) = (Vector.^+^)
  dot = Metric.dot
