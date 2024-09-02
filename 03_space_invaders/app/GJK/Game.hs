{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GJK.Game ( collisionsTable
                , collisions
                , WithCollision, mink
                , CollisionCollection
                , CollisionsC
                ) where

import Data.Bifunctor        (bimap)
import Data.Functor.Compose  (Compose (Compose, getCompose))
import Data.Functor.Product  (Product (Pair))
import Data.Kind             (Constraint)
import Data.Maybe            (fromMaybe)
import Data.Table            (Projection, Table, project)
import Debug.Trace (trace)

import GJK.Mink (Mink)
import qualified GJK.Collision as C
import Witherable as W

collision' :: forall c1 c2 a b.
  (WithCollision c1 a, WithCollision c2 b)
  => a -> b -> Bool
collision' a b = (fromMaybe False) $ C.collision 10 (mink @c1 a) (mink @c2 b)

class WithCollision b a where
  mink :: a -> Mink b

type CollisionCollection f c a = ( WithCollision c a
                                 , W.Filterable f
                                 , Foldable f
                                 ) :: Constraint

type CollisionsC c1 c2 a b f g = ( CollisionCollection f c1 a
                                 , CollisionCollection g c2 b
                                 , Table f g
                                 ) :: Constraint

collisionsTable :: forall c1 c2 a b f g. CollisionsC c1 c2 a b f g
  => f a
  -> g b
  -> (f (a, g b), g (b, f a))
collisionsTable as bs =
  (filter' snd $ uncomp abs, filter' fst $ uncomp bas)
  where
    filter' f = filter'' . fmap (fmap $ W.mapMaybe (fmap f . snd))
    filter'' f = W.mapMaybe (\a@(_, g) -> if null g then Nothing else Just a) f
    comp f = Compose $ fmap (\a -> (a, a)) f
    uncomp f = getCompose $ fmap getCompose f
    (abs, bas) = project collide (comp as, comp bs)
    collide a b = if collision' @c1 @c2 a b then Just (a, b) else Nothing

collisions :: forall c1 c2 a b f g. CollisionsC c1 c2 a b f g
  => f a
  -> g b
  -> (f a, g b)
-- collisions as bs | (trace (show $ length as) False) = undefined
collisions as bs = bimap fst' fst' $ collisionsTable @c1 @c2 as bs
  where fst' f = fmap fst f
