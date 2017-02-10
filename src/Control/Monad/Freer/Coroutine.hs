{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       Control.Monad.Freer.Coroutine
-- Description:  Composable coroutine effects layer.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    broken
-- Portability:  GHC specific language extensions.
--
-- An effect to compose functions with the ability to yield.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Coroutine
    ( Yield(..)
    , yield
    , Status(..)
    , runC
    )
  where

import Control.Monad (return)
import Data.Function (($), (.))
import Data.Functor (Functor)

import Control.Monad.Freer.Internal (Arr, Eff, Member, handleRelay, send)


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
data Status effs a b
    = Done
    -- ^ Coroutine is done.
    | Continue a (b -> Eff effs (Status effs a b))
    -- ^ Reporting a value of the type @a@, and resuming with the value of type
    -- @b@.

-- | Launch a coroutine and report its status.
runC :: Eff (Yield a b ': effs) w -> Eff effs (Status effs a b)
runC = handleRelay (\_ -> return Done) handler
  where
    handler
        :: Yield a b c
        -> Arr effs c (Status effs a b)
        -> Eff effs (Status effs a b)
    handler (Yield a k) arr = return $ Continue a (arr . k)
