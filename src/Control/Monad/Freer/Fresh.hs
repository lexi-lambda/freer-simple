{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

{-|
Module      : Control.Monad.Freer.Fresh
Description : Generation of fresh integers as an effect.
Copyright   : Alej Cabrera 2015
License     : BSD-3
Maintainer  : cpp.cabrera@gmail.com
Stability   : broken
Portability : POSIX

Composable handler for Fresh effects. This is likely to be of use when
implementing De Bruijn naming/scopes.

Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a
starting point.

-}
module Control.Monad.Freer.Fresh (
  Fresh,
  fresh,
  runFresh'
) where

import Control.Monad.Freer.Internal

--------------------------------------------------------------------------------
                             -- Fresh --
--------------------------------------------------------------------------------
-- | Fresh effect model
data Fresh v where
  Fresh :: Fresh Int

-- | Request a fresh effect
fresh :: Member Fresh r => Eff r Int
fresh = send Fresh

-- | Handler for Fresh effects, with an Int for a starting value
runFresh' :: Eff (Fresh ': r) w -> Int -> Eff r w
runFresh' m s =
  handleRelayS s (\_s x -> return x)
                 (\s' Fresh k -> (k $! s'+1) s')
                 m
