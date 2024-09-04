{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Data.Table ( Table, Projection
                  , project, project' ) where

import Data.Bifunctor        (Bifunctor, bimap)
import Data.Functor.Identity (Identity (Identity))
import Data.Kind             (Constraint)
import Data.List             (transpose)
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

project1 :: (Functor f) => (forall d. d -> g d) -> (a -> b -> c) -> a -> f b -> (g (f c), f (g c))
project1 c f a bs = let cs = f a <$> bs in (c cs, c <$> cs)

project1A :: (Applicative g, Functor f) => (a -> b -> c) -> a -> f b -> (g (f c), f (g c))
project1A = project1 pure

instance {-# OVERLAPPABLE #-} Functor f => Projection (,) Maybe f where
  project f (Nothing, bs) = (Nothing, Nothing <$ bs)
  project f ((Just a), bs) = project1A f a bs

instance {-# OVERLAPPABLE #-} Functor f => Projection (,) f Maybe where
  project = project'

instance {-# OVERLAPPABLE #-} Functor f => Projection (,) Identity f where
  project f ((Identity a), bs) = project1A f a bs

instance {-# OVERLAPPABLE #-} Functor f => Projection (,) f Identity where
  project = project'

instance Projection (,) [] [] where
  -- project :: (a -> b -> c) -> ([a], [b]) -> ([[c]], [[c]])
  project f (as, bs) =
    let t = fmap (\a -> fmap (\b -> f a b) bs) as
    in (t, transpose t)
