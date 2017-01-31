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
module Control.Monad.Freer
    (
    -- * Effect Monad
      Eff

    -- ** Effect Constraints
    , Member
    , Members

    -- ** Sending Arbitrary Effect
    , send

    -- * Handling Effects
    , Arr
    , run
    , runM

    -- ** Building Effect Handlers
    , runNat
    , runNatS
    , handleRelay
    , handleRelayS

    -- ** Nondeterminism Effect
    , NonDetEff(..)
    , makeChoiceA
    , msplit
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Function ((.), const)
import Data.Tuple (uncurry)

import Control.Monad.Freer.Internal


runNat
    :: Member m effs
    => (forall a. eff a -> m a)
    -> Eff (eff ': effs) b
    -> Eff effs b
runNat f = handleRelay pure (\b -> (send (f b) >>=))

runNatS
    :: Member m effs
    => s
    -> (forall a. s -> eff a -> m (s, a))
    -> Eff (eff ': effs) w
    -> Eff effs w
runNatS s0 f =
    handleRelayS s0 (const pure) (\s v -> (send (f s v) >>=) . uncurry)
