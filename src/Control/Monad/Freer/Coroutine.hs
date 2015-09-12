{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

-- This module isn't usable yet
module Control.Monad.Freer.Coroutine (
  Yield,
  yield,
  Y(..)
) where

import Control.Monad.Freer.Internal


-- The yield request: reporting the value of type a and suspending
-- the coroutine. Resuming with the value of type b
data Yield a b v = Yield a (b -> v)
    deriving (Functor)

-- The signature is inferred
yield :: (Member (Yield a b) r) => a -> (b -> c) -> Eff r c
yield x f = send (Yield x f)

-- Status of a thread: done or reporting the value of the type a
-- and resuming with the value of type b
data Y r a b = Done | Y a (b -> Eff r (Y r a b))

{- FIXME: this does not compile
-- Launch a thread and report its status
runC :: Eff (Yield a b ': r) w -> Eff r (Y r a b)
runC m = loop m
  where loop :: Monad m => Eff a b -> m (Y r c d)
        loop (Val _)  = return Done
        loop (E u u') = handleRelay u loop $
                          \(Yield x k) -> return (Y x (loop . k))
-}
