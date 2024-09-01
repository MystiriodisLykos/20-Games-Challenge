{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Table.DMap where

import Prelude hiding (lookup)

import qualified Data.Map.Strict as Map

import Witherable as W

import Data.DMap (DMap (DMap, getDefault), lookup, mapWithKey)
import Data.Table (Projection, project)

instance forall k1 k2. (Ord k2) => Projection (,) (DMap k1) (DMap k2) where
  -- project :: forall a b c. (a -> b -> c)
  --   -> (DMap k1 a, DMap k2 b)
  --   -> (DMap k1 (DMap k2 c), DMap k2 (DMap k1 c))
  project f (as, bs) =
    let
      -- ab :: DMap k1 (DMap k2 c)
      ab = (\a -> f a <$> bs) <$> as
      -- bd :: b -> Maybe (DMap k1 c) -- DMap pink red
      bd _ = Just $ W.catMaybes $ getDefault <$> ab
      -- br :: k2 -> b -> DMap k1 c -- DMap orange (blue | green)
      br bk _ = W.catMaybes $ (lookup bk) <$> ab
      -- ba :: DMap k2 (DMap k1 c)
      ba = mapWithKey bd br bs
    in (ab, ba)

