{-# LANGUAGE FlexibleContexts #-}
module State where

import Control.Monad.Freer
import Control.Monad.Freer.State

--------------------------------------------------------------------------------
                       -- Tests and Examples --
--------------------------------------------------------------------------------
ts1 :: Member (State Int) r => Eff r Int
ts1 = do
  put (10 ::Int)
  x <- get
  return (x::Int)

ts1r :: Bool
ts1r = ((10,10) ==) $ run (runState ts1 (0::Int))

ts2 :: Member (State Int) r => Eff r Int
ts2 = do
  put (10::Int)
  x <- get
  put (20::Int)
  y <- get
  return (x+y)

ts2r :: Bool
ts2r = ((30,20) ==) $ run (runState ts2 (0::Int))
