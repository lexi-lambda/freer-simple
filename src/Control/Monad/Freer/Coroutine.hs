{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

{-|
Module      : Control.Monad.Freer.Coroutine
Description : Composable coroutine effects layer.
Copyright   : Allele Dev 2016
License     : BSD-3
Maintainer  : allele.dev@gmail.com
Stability   : broken
Portability : POSIX

An effect to compose functions with the ability to yield.

Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a
starting point.

-}
module Control.Monad.Freer.Coroutine (
  Yield,
  yield,
  Status(..),
  runC
) where

import Control.Monad.Freer.Internal

-- | A type representing a yielding of control
-- a: The current type
-- b: The input to the continuation function
-- v: The output of the continuation
data Yield a b v = Yield a (b -> v)
    deriving (Functor)

-- | Lifts a value and a function into the Coroutine effect
yield :: (Member (Yield a b) r) => a -> (b -> c) -> Eff r c
yield x f = send (Yield x f)

-- |
-- Status of a thread: done or reporting the value of the type a and
-- resuming with the value of type b
data Status r a b = Done | Continue a (b -> Eff r (Status r a b))

-- | Launch a thread and report its status
runC :: Eff (Yield a b ': r) w -> Eff r (Status r a b)
runC = handleRelay (\_ -> return Done) handler
  where
    handler :: Yield a b v -> Arr r v (Status r a b) -> Eff r (Status r a b)
    handler (Yield a k) arr = return $ Continue a (arr . k)
