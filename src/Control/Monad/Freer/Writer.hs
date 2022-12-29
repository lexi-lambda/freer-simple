{-# LANGUAGE CPP #-}

-- |
-- Module:       Control.Monad.Freer.Writer
-- Description:  Composable Writer effects.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King
-- License:      BSD3
-- Maintainer:   Alexis King <lexi.lambda@gmail.com>
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- 'Writer' effects, for writing\/appending values (line count, list of
-- messages, etc.) to an output. The current value of the 'Writer' effect output
-- is not accessible to the computation.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Writer
  ( Writer(..)
  , tell
  , runWriter
  ) where

import Control.Arrow (second)

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif

import Control.Monad.Freer.Internal (Eff, Member, handleRelay, send)

-- | Writer effects - send outputs to an effect environment.
data Writer w r where
  Tell :: w -> Writer w ()

-- | Send a change to the attached environment.
tell :: forall w effs. Member (Writer w) effs => w -> Eff effs ()
tell w = send (Tell w)

-- | Simple handler for 'Writer' effects.
runWriter :: forall w effs a. Monoid w => Eff (Writer w ': effs) a -> Eff effs (a, w)
runWriter = handleRelay (\a -> pure (a, mempty)) $ \(Tell w) k ->
  second (w <>) <$> k ()
