{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Control.Monad.Freer.StateRW (
  runStateR
) where

import Control.Monad.Freer.Reader
import Control.Monad.Freer.Writer
import Control.Monad.Freer.Internal

--------------------------------------------------------------------------------
                 -- State: Using Reader and Writer --
--------------------------------------------------------------------------------
-- A different representation of State: decomposing State into
-- mutation (Writer) and Reading. We don't define any new effects:
-- we just handle the existing ones.
-- Thus we define a handler for two effects together.

runStateR :: Eff (Writer s ': Reader s ': r) w -> s -> Eff r (w,s)
runStateR m s = loop s m
 where
   loop :: s -> Eff (Writer s ': Reader s ': r) w -> Eff r (w,s)
   loop s (Val x) = return (x,s)
   loop s (E u q) = case decomp u of
     Right (Writer o) -> k o ()
     Left  u  -> case decomp u of
       Right Reader -> k s s
       Left u -> E u (tsingleton (k s))
    where k s = qComp q (loop s)

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
