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
-- messages, etc.) to an output. Current value of 'Writer' effect output is not
-- accessible to the computation.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Writer
  ( Writer(..)
  , tell
  , runWriter
  ) where

import Data.Monoid ((<>))

import Control.Monad.Freer.Internal (Eff, Member, send)
import Control.Monad.Freer.Interpretation
import Control.Monad.Trans.State.Strict (modify)

-- | Writer effects - send outputs to an effect environment.
data Writer w r where
  Tell :: w -> Writer w ()

-- | Send a change to the attached environment.
tell :: forall w effs. Member (Writer w) effs => w -> Eff effs ()
tell w = send (Tell w)

-- | Simple handler for 'Writer' effects.
runWriter :: forall w effs a. Monoid w => Eff (Writer w ': effs) a -> Eff effs (a, w)
runWriter = withStateful mempty $ \(Tell w) -> modify (<> w)
