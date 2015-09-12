{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Control.Monad.Freer.Fresh (
  Fresh,
  fresh,
  runFresh'
) where

import Control.Monad.Freer.Internal

--------------------------------------------------------------------------------
                             -- Fresh --
--------------------------------------------------------------------------------
data Fresh v where
  Fresh :: Fresh Int

fresh :: Member Fresh r => Eff r Int
fresh = send Fresh

-- And a handler for it
runFresh' :: Eff (Fresh ': r) w -> Int -> Eff r w
runFresh' m s =
  handleRelayS s (\_s x -> return x)
                 (\s' Fresh k -> (k $! s'+1) s')
                 m
