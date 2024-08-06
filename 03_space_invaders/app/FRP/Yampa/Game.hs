{-# LANGUAGE Arrows #-}

module FRP.Yampa.Game ( WithKillFlag (..)
                      , pKillSpawn, spawnC
                      , switchWhen, switchWhenE
                      , switchAfter, onlyEvery
                      , ) where

import Control.Arrow       ( (>>>), (^>>), (>>^), (&&&)
                           , arr, first, second, returnA
                           )
import Control.Applicative ( Alternative, (<|>) )
import Data.Bool           (bool)

import FRP.Yampa          ( SF, Event (Event, NoEvent), Time
                          , constant
                          )
import FRP.Yampa.Event    ( catEvents, tag )
import FRP.Yampa.EventS   ( after )
import FRP.Yampa.Switches ( switch, dSwitch )
import Witherable as W

import FRP.Yampa.AltSwitches ( drpSwitchA, safePair )

class WithKillFlag a where
  killF :: a -> Bool

instance (Foldable f) => WithKillFlag (f a) where
  killF = null

filterBy :: (Alternative col, W.Filterable col) => col Bool -> col a -> col a
filterBy bs as = W.mapMaybe map' $ safePair True bs as
  where
    map' (True, a) = Just a
    map' _          = Nothing

mergeC :: [Event (a -> a)] -> Event (a -> a)
mergeC = (fmap $ foldl1 (.)) . catEvents

switchWhen :: SF a b -> SF (a, Event c) (Maybe b)
switchWhen sf = switch (first $ sf >>^ Just) (const $ constant Nothing)

switchWhenE :: SF () b -> SF (Event a) (Maybe b)
switchWhenE sf = ((,) ()) ^>> switchWhen sf

switchAfter :: Time -> SF a b -> SF a b -> SF a b
switchAfter n a b = switch (a &&& after n b) id

onlyEvery :: Time -> SF a (Event b) -> SF a (Event b)
onlyEvery t sf = dSwitch (sf >>^ (\e -> (e, e `tag` ()))) cont
  where
    cont _ = switch (after t () >>^ (\e -> (NoEvent, e))) (const $ onlyEvery t sf)

drpKillSwitch :: (WithKillFlag b, Alternative col, W.Filterable col, Foldable col)
  => a
  -> col (SF a b)
  -> SF (col a, Event (col (SF a b) -> col (SF a b))) (col b)
drpKillSwitch a sfs = proc (as, e) -> do
  rec
    bs <- drpSwitchA a sfs -< (as, mergeC [killE bs, e])
  returnA -< bs
  where
    killE bs = event' $ (not . killF) <$> bs
    -- event' bs | (trace (show $ and bs) False) = undefined
    event' bs = bool (Event $ filterBy bs) NoEvent (and bs)

spawnC :: (WithKillFlag b, Alternative col, W.Filterable col, Foldable col)
  => SF (Event (col (SF a b))) (Event (col (SF a b) -> col (SF a b)))
spawnC = arr $ fmap $ flip (<|>)

pKillSpawn :: (WithKillFlag b, Alternative col, W.Filterable col, Foldable col)
  => a
  -> col (SF a b)
  -> SF (col a, Event (col (SF a b))) (col b)
pKillSpawn a sfs = second spawnC >>> drpKillSwitch a sfs