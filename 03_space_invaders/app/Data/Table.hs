{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Table ( Table, project ) where

import Data.List (transpose)

class (Functor f, Functor g) => Table f g where
  project :: (a -> b -> c) -> f a -> g b -> (f (g c), g (f c))
  project f as bs = ( fmap (\a -> fmap (\b -> f a b) bs) as
                    , fmap (\b -> fmap (\a -> f a b) as) bs
                    )

listMaybeProjection :: (a -> b -> c) -> [a] -> Maybe b -> ([Maybe c], Maybe [c])
listMaybeProjection f as Nothing = (Nothing <$ as, Nothing)
listMaybeProjection f as (Just b) =
  let as' = (\a -> f a b) <$> as
  in (Just <$> as', Just as')

instance Table [] Maybe where
  project = listMaybeProjection

instance Table Maybe [] where
  project :: (a -> b -> c) -> Maybe a -> [b] -> (Maybe [c], [Maybe c])
  project f as bs =
    let (l, m) = project (flip f) bs as
    in (m, l)

instance Table [] [] where
  project :: (a -> b -> c) -> [a] -> [b] -> ([[c]], [[c]])
  project f as bs = 
    let t = fmap (\a -> fmap (\b -> f a b) bs) as
    in (t, transpose t)

