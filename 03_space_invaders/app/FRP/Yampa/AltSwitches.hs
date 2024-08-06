module FRP.Yampa.AltSwitches (
  parA,
  pSwitchA, dpSwitchA,
  rpSwitchA, drpSwitchA,
  safePair) where

import FRP.Yampa          ( SF, Event )
import FRP.Yampa.Switches ( par, pSwitch, dpSwitch, rpSwitch, drpSwitch )

import Control.Applicative ( Alternative, (<|>) )

import Debug.Trace (trace)

safePair :: (Alternative col) => a -> col a -> col sf -> col (a, sf)
-- safePair _ _ sfs | (trace (show $ length sfs) False) = undefined
safePair a is sfs = (,) <$> (is <|> pure a) <*> sfs

parA :: (Alternative col) => a -> col (SF a b) -> SF (col a) (col b)
parA a = par (safePair a)

pSwitchA :: (Alternative col)
  => a
  -> col (SF a b)
  -> SF (col a, col b) (Event c)
  -> (col (SF a b) -> c -> SF (col a) (col b))
  -> SF (col a) (col b)
pSwitchA a = pSwitch (safePair a)

dpSwitchA :: (Alternative col)
  => a
  -> col (SF a b)
  -> SF (col a, col b) (Event c)
  -> (col (SF a b) -> c -> SF (col a) (col b))
  -> SF (col a) (col b)
dpSwitchA a = dpSwitch (safePair a)

rpSwitchA :: (Alternative col)
  => a
  -> col (SF a b)
  -> SF (col a, Event (col (SF a b) -> col (SF a b))) (col b)
rpSwitchA a = rpSwitch (safePair a)

drpSwitchA :: (Alternative col)
  => a
  -> col (SF a b)
  -> SF (col a, Event (col (SF a b) -> col (SF a b))) (col b)
drpSwitchA a = drpSwitch (safePair a)
