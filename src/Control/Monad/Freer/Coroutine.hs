-- |
-- Module:       Control.Monad.Freer.Coroutine
-- Description:  Composable coroutine effects layer.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King
-- License:      BSD3
-- Maintainer:   Alexis King <lexi.lambda@gmail.com>
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- An effect to compose functions with the ability to yield.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Coroutine
  ( -- * Yield Control
    Yield(..)
  , yield

    -- * Handle Yield Effect
  , Status(..)
  , runC
  , interposeC
  , replyC
  ) where

import Control.Monad.Freer.Internal (Eff, Member, HasLen, handleRelay, interpose, send)

-- | A type representing a yielding of control.
--
-- Type variables have following meaning:
--
-- [@a@]
--   The current type.
--
-- [@b@]
--   The input to the continuation function.
--
-- [@c@]
--   The output of the continuation.
data Yield a b c = Yield a (b -> c)
  deriving (Functor)

-- | Lifts a value and a function into the Coroutine effect.
yield :: Member (Yield a b) effs => a -> (b -> c) -> Eff effs c
yield x f = send (Yield x f)

-- | Represents status of a coroutine.
data Status effs a b r
  = Done r
  -- ^ Coroutine is done with a result value of type @r@.
  | Continue a (b -> Eff effs (Status effs a b r))
  -- ^ Reporting a value of the type @a@, and resuming with the value of type
  -- @b@, possibly ending with a value of type @x@.

-- | Reply to a coroutine effect by returning the Continue constructor.
replyC
  :: Yield a b c
  -> (c -> Eff effs (Status effs a b r))
  -> Eff effs (Status effs a b r)
replyC (Yield a k) arr = pure $ Continue a (arr . k)

-- | Launch a coroutine and report its status.
runC :: HasLen effs => Eff (Yield a b ': effs) r -> Eff effs (Status effs a b r)
runC = handleRelay (pure . Done) replyC

-- | Launch a coroutine and report its status, without handling (removing)
-- 'Yield' from the typelist. This is useful for reducing nested coroutines.
interposeC
  :: Member (Yield a b) effs
  => Eff effs r
  -> Eff effs (Status effs a b r)
interposeC = interpose (pure . Done) replyC
