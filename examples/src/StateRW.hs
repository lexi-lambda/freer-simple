{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module StateRW where

import Control.Monad.Freer
import Control.Monad.Freer.StateRW

--------------------------------------------------------------------------------
                       -- Tests and Examples --
--------------------------------------------------------------------------------
-- If we had a Writer, we could have decomposed State into Writer and Reader
-- requests.

ts11 :: (Member (Reader Int) r, Member (Writer Int) r) => Eff r Int
ts11 = do
  tell (10 ::Int)
  x <- ask
  return (x::Int)

ts11r :: Bool
ts11r = ((10,10) ==) $ run (runStateR ts11 (0::Int))


ts21 :: (Member (Reader Int) r, Member (Writer Int) r) => Eff r Int
ts21 = do
  tell (10::Int)
  x <- ask
  tell (20::Int)
  y <- ask
  return (x+y)

ts21r :: Bool
ts21r = ((30,20) ==) $ run (runStateR ts21 (0::Int))
