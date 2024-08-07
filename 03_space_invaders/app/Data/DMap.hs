module Data.DMap ( elems, fromList
                 , DMap (..)
                 , IMap (..)
                 , liftMap, liftMapF, liftMapBi
                 , partition
                 ) where

import qualified Data.Map.Strict as Map
import Control.Applicative                ( Alternative, empty, (<|>) )
import Data.Bifunctor (Bifunctor, bimap)

import Witherable as W

data DMap k v = DMap
  { getDefault :: Maybe v
  , toMap   :: Map.Map k v
  } deriving Show

type IMap v = DMap Int v

elems :: DMap k v -> [v]
elems = Map.elems . toMap

fromList :: [v] -> IMap v
fromList as = DMap Nothing $ Map.fromAscList $ zip [0..] as

liftMap :: (Map.Map k v -> Map.Map k v) -> DMap k v -> DMap k v
liftMap f (DMap d m) = DMap d $ f m

liftMapF :: (Functor f)
  => (Map.Map k v -> f (Map.Map k v))
  -> DMap k v
  -> f (DMap k v)
liftMapF f (DMap d m) = (DMap d) <$> (f m)

liftMapBi :: (Bifunctor f)
  => (Map.Map k v -> f (Map.Map k v) (Map.Map k v))
  -> DMap k v
  -> f (DMap k v) (DMap k v)
liftMapBi f (DMap d m) = bimap c c $ f m
  where c = DMap d

-- partition :: (v -> Bool) -> DMap k v -> (DMap k v, DMap k v)
partition p = liftMapBi $ Map.partition p

instance Functor (DMap k) where
  fmap f (DMap d m) = DMap (f <$> d) (fmap f m)

instance (Ord k) => Applicative (DMap k) where
  pure x = DMap (Just x) Map.empty

  (<*>) :: DMap k (a -> b) -> DMap k a -> DMap k b
  DMap df fs <*> DMap dx xs = DMap (df <*> dx) $ Map.unions
    [ Map.intersectionWith ($) fs xs, fFallback, xFallback ]
    where
      fFallback =
        case df of
          Nothing -> mempty
          Just f  -> fmap f xs

      xFallback =
        case dx of
          Nothing -> mempty
          Just x  -> fmap ($ x) fs

instance (Ord k) => Alternative (DMap k) where
  empty = DMap Nothing Map.empty

  DMap dl ml <|> DMap dr mr = DMap (dl <|> dr) (Map.union ml mr)

instance W.Filterable (DMap k) where
  mapMaybe f (DMap d m) = DMap (d >>= f) (mapMaybe f m)

instance Foldable (DMap k) where
  foldMap f (DMap d m) = maybe mempty f d <> foldMap f m

