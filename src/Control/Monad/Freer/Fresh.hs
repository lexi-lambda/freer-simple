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
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Composable handler for 'Fresh' effects. This is likely to be of use when
-- implementing De Bruijn naming/scopes.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.

module Control.Monad.Freer.Fresh
    ( Fresh(..)
    , fresh
    , runFresh
    , evalFresh
    , runFresh'
    )
  where

import Prelude (($!), (+))

import Control.Applicative (pure)
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Tuple (fst)

import Control.Monad.Freer.Internal (Eff, Member, handleRelayS, send)


-- | Fresh effect model.
data Fresh a where
    Fresh :: Fresh Int

-- | Request a fresh effect.
fresh :: Member Fresh effs => Eff effs Int
fresh = send Fresh

-- | Handler for 'Fresh' effects, with an 'Int' for a starting value. The
-- return value includes the next fresh value.
runFresh :: Eff (Fresh ': effs) a -> Int -> Eff effs (a, Int)
runFresh m s =
    handleRelayS s (\s' a -> pure (a, s')) (\s' Fresh k -> (k $! s' + 1) s') m

-- | Handler for 'Fresh' effects, with an 'Int' for a starting value. Discards
-- the next fresh value.
evalFresh :: Eff (Fresh ': effs) a -> Int -> Eff effs a
evalFresh = ((fst <$>) .) . runFresh

-- | Backward compatibility alias for 'evalFresh'.
runFresh' :: Eff (Fresh ': effs) a -> Int -> Eff effs a
runFresh' = evalFresh
{-# DEPRECATED runFresh'
    "Use `evalFresh` instead, this function will be removed in next release."
  #-}
