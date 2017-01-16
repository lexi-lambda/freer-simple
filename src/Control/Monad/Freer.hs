{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-|
Module      : Control.Monad.Freer
Description : Freer - an extensible effects library
Copyright   : Allele Dev 2016
License     : BSD-3
Maintainer  : allele.dev@gmail.com
Stability   : experimental
Portability : POSIX

-}
module Control.Monad.Freer (
  Member,
  Members,
  Eff,
  run,
  runM,
  runNat,
  handleRelay,
  handleRelayS,
  send,
  Arr,

  NonDetEff(..),
  makeChoiceA,
  msplit
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (pure)
#endif

import Control.Monad.Freer.Internal

runNat
  :: Member m r
  => (forall a. e a -> m a) -> Eff (e ': r) w -> Eff r w
runNat f = handleRelay pure (\v -> (send (f v) >>=))
