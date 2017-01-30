{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       Control.Monad.Freer
-- Description:  Freer - an extensible effects library
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  POSIX
module Control.Monad.Freer (
  Member,
  Members,
  Eff,
  run,
  runM,
  runNat,
  runNatS,
  handleRelay,
  handleRelayS,
  send,
  Arr,

  NonDetEff(..),
  makeChoiceA,
  msplit
) where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Function ((.), const)
import Data.Tuple (uncurry)

import Control.Monad.Freer.Internal


runNat
  :: Member m r
  => (forall a. e a -> m a) -> Eff (e ': r) w -> Eff r w
runNat f = handleRelay pure (\v -> (send (f v) >>=))

runNatS
  :: Member m r
  => s -> (forall a. s -> e a -> m (s, a)) -> Eff (e ': r) w -> Eff r w
runNatS s0 f =
  handleRelayS s0 (const pure) (\s v -> (send (f s v) >>=) . uncurry)
