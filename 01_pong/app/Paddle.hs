{-# LANGUAGE Arrows #-}

module Paddle( PaddleDirection(..)
             , PaddleInput(..)
             , PaddleState(..)
             , paddle, drawPaddle
             ) where

import Control.Arrow                      ( returnA )
import FRP.Yampa                          ( SF, integral)
import Graphics.Gloss                     ( Picture (Color,Translate)
                                          , Color
                                          , rectangleSolid)
import Linear.V2   ( V2 (V2) )

import Linear.VectorSpace()

data PaddleDirection = PaddleUp | PaddleDown | PaddleStop deriving Show

data PaddleInput = PaddleInput {
  paddleDirection :: PaddleDirection
}

data PaddleState = PaddleState {
  pP :: V2 Float,
  pV :: Float,
  pS :: V2 Float,
  pColor :: Color
}

paddle' :: PaddleState -> SF PaddleInput PaddleState
paddle' initial = proc pi' -> do
  y <- integral -< (pV initial) * (v $ paddleDirection pi')
  returnA -< initial {pP = (V2 0 y) + pP initial}
  where
    v PaddleUp = 1
    v PaddleStop = 0
    v PaddleDown = -1

paddle :: PaddleState -> SF (a, PaddleState) PaddleInput -> SF a PaddleState
paddle initial pi' = proc i -> do
  rec
    pi' <- pi' -< (i, ps)
    ps <- paddle' initial -< pi'
  returnA -< ps

drawPaddle :: PaddleState -> Picture
drawPaddle (PaddleState (V2 x y) _ (V2 w h) color) = Color color $ Translate x y $ rectangleSolid w h
