{-# LANGUAGE TypeFamilies #-}

module Linear.MemoTrie where

import Control.Arrow (first)

import Control.Newtype.Generics (Newtype, pack, unpack, O)

import Linear.V2 (V2(V2))
import Data.MemoTrie (HasTrie, (:->:), trie, untrie, enumerate)

instance HasTrie a => HasTrie (V2 a) where
  newtype (V2 a) :->: x = V2Trie ((a, a) :->: x)
  -- trie :: (V2 a -> b) -> V2 a :->: b
  trie f = V2Trie $ trie $ f . v2
  -- untrie :: (V2 a :->: b) -> V2 a -> b
  untrie (V2Trie t) = untrie t . unV2
    where unV2 (V2 a b) = (a, b)
  -- enumerate :: (V2 a :->: b) -> [(V2 a, b)]
  enumerate (V2Trie t) = (fmap . first) v2 (enumerate t)

v2 :: (a, a) -> V2 a
v2 (a, b) = V2 a b
