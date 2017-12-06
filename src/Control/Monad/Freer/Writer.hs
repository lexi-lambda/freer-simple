-- |
-- Module:       Control.Monad.Freer.Writer
-- Description:  Composable Writer effects.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- 'Writer' effects, for writing\/appending values (line count, list of
-- messages, etc.) to an output. Current value of 'Writer' effect output is not
-- accessible to the computation.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Writer
  ( Writer(..)
  , tell
  , runWriter
  ) where

import Control.Arrow (second)
import Data.Monoid ((<>))

import Control.Monad.Freer.Internal (Eff, Member, handleRelay, send)

-- | Writer effects - send outputs to an effect environment.
data Writer w a where
  Writer :: w -> Writer w ()

-- | Send a change to the attached environment.
tell :: Member (Writer w) effs => w -> Eff effs ()
tell w = send $ Writer w

-- | Simple handler for 'Writer' effects.
runWriter :: Monoid w => Eff (Writer w ': effs) a -> Eff effs (a, w)
runWriter = handleRelay (\a -> pure (a, mempty)) $ \(Writer w) k ->
  second (w <>) <$> k ()
