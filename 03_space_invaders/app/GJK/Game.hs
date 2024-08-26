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
