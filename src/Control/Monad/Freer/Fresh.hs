{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       Control.Monad.Freer.Fresh
-- Description:  Generation of fresh integers as an effect.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    broken
-- Portability:  POSIX
--
-- Composable handler for 'Fresh' effects. This is likely to be of use when
-- implementing De Bruijn naming/scopes.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Fresh (
  Fresh(..),
  fresh,
  runFresh'
) where

import Prelude (($!), (+))

import Control.Applicative (pure)
import Data.Int (Int)

import Control.Monad.Freer.Internal (Eff, Member, handleRelayS, send)


--------------------------------------------------------------------------------
                             -- Fresh --
--------------------------------------------------------------------------------

-- | Fresh effect model
data Fresh a where
  Fresh :: Fresh Int

-- | Request a fresh effect
fresh :: Member Fresh effs => Eff effs Int
fresh = send Fresh

-- | Handler for 'Fresh' effects, with an 'Int' for a starting value.
runFresh' :: Eff (Fresh ': effs) a -> Int -> Eff effs a
runFresh' m s =
    handleRelayS s (\_s a -> pure a) (\s' Fresh k -> (k $! s' + 1) s') m
