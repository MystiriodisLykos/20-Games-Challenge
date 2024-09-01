{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Table ( Table, Projection
                  , project, project' ) where

import Data.Bifunctor (Bifunctor, bimap)
import Data.Kind      (Constraint)
import Data.List      (transpose)
import Debug.Trace

class Projection b f g where
  project :: (x -> y -> z) -> b (f x) (g y) -> b (f (g z)) (g (f z))

type Table f g = Projection (,) f g :: Constraint

instance {-# OVERLAPPABLE #-} (Functor f, Functor g) => Projection (,) f g where
  -- project :: (a -> b -> c) -> (f a, g b) -> (f (g c), g (f c))
  project f (as, bs) = ( fmap (\a -> fmap (\b -> f a b) bs) as
                       , fmap (\b -> fmap (\a -> f a b) as) bs
                       )

project' :: (Functor f, Functor g, Table f g)
  => (b -> a -> c)
  -> (g b, f a)
  -> (g (f c), f (g c))
project' f (bs, as) = let (a, b) = project (flip f) (as, bs) in (b, a)

listMaybeProjection :: (a -> b -> c) -> ([a], Maybe b) -> ([Maybe c], Maybe [c])
listMaybeProjection f (as, Nothing) = (Nothing <$ as, Nothing)
listMaybeProjection f (as, (Just b)) =
  let as' = (\a -> f a b) <$> as
  in (Just <$> as', Just as')

instance Projection (,) [] Maybe where
  project = listMaybeProjection

instance Projection (,) Maybe [] where
  project f = project' f

instance Projection (,) [] [] where
  -- project :: (a -> b -> c) -> ([a], [b]) -> ([[c]], [[c]])
  project f (as, bs) =
    let t = fmap (\a -> fmap (\b -> f a b) bs) as
    in (t, transpose t)
