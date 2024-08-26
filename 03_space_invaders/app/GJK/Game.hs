{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module GJK.Game ( collisions
                , WithCollision', mink
                ) where

import Data.Functor.Compose  (Compose (Compose, getCompose))
import Data.Functor.Product  (Product (Pair))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Functor.Const    (Const (Const, getConst))
import Data.Monoid           (First (First, getFirst))
import Data.Profunctor
import Data.Maybe            (fromMaybe)
import Data.Table            (Projection, Table, project)

import GJK.Mink (Mink)
import qualified GJK.Collision as C
import Witherable as W

import Control.Monad.Trans.Maybe
import Control.Monad (join)

collision :: Mink a -> Mink b -> Bool
collision a = (fromMaybe False) . (C.collision 10 a)

collision' :: forall c1 c2 a b.
  (WithCollision' c1 a, WithCollision' c2 b)
  => a -> b -> Bool
collision' a b = (fromMaybe False) $ C.collision 10 (mink @c1 a) (mink @c2 b)

class WithCollision' b a where
  mink :: a -> Mink b

collisions :: forall c1 c2 a b f g. (
  WithCollision' c1 a, WithCollision' c2 b,
  Table f g, W.Filterable f, W.Filterable g,
  Foldable f, Foldable g)
  => f a
  -> g b
  -> (f (a, g b), g (b, f a))
collisions as bs =
  (filter' snd $ uncomp abs, filter' fst $ uncomp bas)
  where
    filter' f = filter'' . fmap (fmap $ W.mapMaybe (fmap f . snd))
    filter'' f = W.mapMaybe (\a@(_, g) -> if null g then Nothing else Just a) f
    comp f = Compose $ fmap (\a -> (a, a)) f
    uncomp f = getCompose $ fmap getCompose f
    (abs, bas) = project collide (comp as, comp bs)
    collide a b = if collision' @c1 @c2 a b then Just (a, b) else Nothing

-- What about Product g Maybe
-- I really feel it in my gut that the Foldable constraints are not needed
-- This is essentially (g a, Maybe a), then I could filter off the maybe?
-- So I would need a projection like this?
-- project' :: _ -> _ -> (Compose f ((,) a)) (Product g Maybe a)
--                    f (a, (g a, Maybe a))
-- g a = Product g' Maybe a
-- f a = Compose f' ((,) a)

-- f (Maybe a, a)
comp' :: (Functor f) => f a -> Compose f (Product Maybe Identity) a
comp' = Compose . fmap (\a -> Pair (Just a) (Identity a))

-- (f (Maybe a, a), g (Maybe b, b)) -> (f (Maybe a, g (a, b)), _)

-- collisions' :: forall c1 c2 a b f g. (
--   WithCollision' c1 a, WithCollision' c2 b,
--   Table f g, W.Filterable f, W.Filterable g
--   )
--   => f a
--   -> g b
--   -- collisions indexed by f, collisions indexed by g
--   -> (f (Maybe (a, b)), g (Maybe (a, b)))
-- collisions' as bs = project (\a b -> (a, b)) (as, bs)

-- data T f a = T {m' :: Maybe (), f' :: f a}

-- newtype T f a = T (f (Maybe (), a))

-- projection :: (a -> b -> Maybe c) -> _ -> (f (Maybe a, g c), g (Maybe b, f c))
-- f a = Compose f (Product (Constant Maybe) Identity) a
-- f a = f (Product Maybe Identity) a
-- f a = f (Maybe a, Identity a)

-- comp'' :: (Functor f) => f a -> Compose f (Product (Const (Maybe a)) Identity) a
-- comp'' f = Compose $ (\a -> Pair (Const $ Just a) (Identity a)) <$> f

comp'' :: (Functor f) => f a -> Compose f (Product First Identity) a
comp'' f = Compose $ (\a -> Pair (First $ Just a) (Identity a)) <$> f

-- newtype M f g a = M (f (g a))

-- instance (Functor f) => Functor (M f Maybe) where

-- I want something like (W.Filterable f) => (Maybe a, f a) where
-- map (a -> Maybe b) (Maybe a, f a) = (Maybe b, f b)
-- map (a -> Just b ) (Nothing, f a) = (Just b , f b)
-- map (a -> Just b ) (Just a , f a) = (Just b , f b)
-- map (a -> Nothing) (Just a , f a) = (Nothing , f b)

data M f b a = M (a -> Maybe b) (Maybe b) (f a)

-- mmap :: (a -> b) -> M f a -> Maybe a
-- mmap f (M c (Just a) as) = join $ c <$> as

mmap :: (Functor f) => (a -> c) -> (c -> a) -> M f b a -> M f b c
mmap f g (M c (Just a) as) =
  let
    c' = c . g
    cs = f <$> as
    ms = c' <$> cs
  in M c' (Just a) $ cs

-- instance (Functor f) => Profunctor (M f b) where
--   dimap = undefined
  -- fmap f (M c (Just a) as) = join $ c <$> as
  -- fmap f (M c Nothing as) = M c Nothing $ f <$> as

-- instance (Functor f, Functor g) => Projection (,) (M f) (M g) where
--   project :: (a -> b -> c)
--     -> (M f a, M g b)
--     -> (M f (M g c), M g (M f c))
--   project = undefined

p' :: (Table f g)
  => (a -> b -> Maybe c)
  -> (f a, g b)
  -> (f (g (Maybe c)), g (f (Maybe c)))
p' = project

-- f :: a -> b -> Maybe ((a, b), c)
-- fmap (\b -> f a b) (Something [1,2,3])
-- = (Maybe (a, b), [c])

-- const' :: (Functor f)
--   => Compose f (Product (Const (Maybe a)) Identity) b
--   -- -> f ((Product (Const (Maybe a)) Identity) a)
--   -> f (Maybe a)
--   -- -> Compose f Maybe a
-- const' (Compose f) = (\(Pair m _) -> getConst m) <$> f

-- instance (Functor f) => Functor (T f) where
--   fmap f (T fs) = T ((fmap f) <$> fs)

-- instance (Table f g) => Projection (,) (T f) (T g) where
--   project :: (a -> b -> Maybe c) -> (T f a, T g b) -> (T f (T g c), T g (T f c))
--   project f (as, bs) = undefined

-- projection (T f a, T g b) => 

-- projection :: (Table f g) => (f a, g b) -> (f (g (Maybe ())), g (f (Maybe ())))
-- projection = project (\a b -> Nothing)
