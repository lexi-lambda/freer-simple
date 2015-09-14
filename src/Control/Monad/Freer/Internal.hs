{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- The following is needed to define MonadPlus instance. It is decidable
-- (there is no recursion!), but GHC cannot see that.
-- TODO: remove once GHC can deduce the decidability of this instance
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Control.Monad.Freer.Internal
Description : Mechanisms to make effects work
Copyright   : Alej Cabrera 2015
License     : BSD-3
Maintainer  : cpp.cabrera@gmail.com
Stability   : experimental
Portability : POSIX

Internal machinery for this effects library. This includes:

* Eff data type, for expressing effects
* NonDetEff data type, for nondeterministic effects
* Functions for facilitating the construction of effects and their handlers

Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a
starting point.

-}
module Control.Monad.Freer.Internal (
  Eff(..),
  Member(..),
  Arr,
  Arrs,
  Union,

  NonDetEff(..),
  makeChoiceA,
  msplit,

  decomp,
  tsingleton,

  qApp,
  qComp,
  send,
  run,
  handleRelay,
  handleRelayS,
  interpose,
) where

import Control.Monad
import Control.Applicative
import Data.Open.Union
import Data.FTCQueue


-- |
-- Effectful arrow type: a function from a to b that also does effects
-- denoted by r
type Arr r a b = a -> Eff r b

-- |
-- An effectful function from 'a' to 'b' that is a composition of
-- several effectful functions. The paremeter r describes the overall
-- effect. The composition members are accumulated in a type-aligned
-- queue.
type Arrs r a b = FTCQueue (Eff r) a b

-- |
-- The Eff representation.
--
-- Status of a coroutine (client):
-- * Val: Done with the value of type a
-- * E  : Sending a request of type Union r with the continuation Arrs r b a
data Eff r a = Val a
             | forall b. E (Union r b) (Arrs r b a)

-- | Function application in the context of an array of effects, Arrs r b w
qApp :: Arrs r b w -> b -> Eff r w
qApp q' x =
   case tviewl q' of
   TOne k  -> k x
   k :| t -> case k x of
     Val y -> qApp t y
     E u q -> E u (q >< t)

-- | Composition of effectful arrows
-- Allows for the caller to change the effect environment, as well
qComp :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arr r' a c
qComp g h a = h $ qApp g a

instance Functor (Eff r) where
  {-# INLINE fmap #-}
  fmap f (Val x) = Val (f x)
  fmap f (E u q) = E u (q |> (Val . f))

instance Applicative (Eff r) where
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}
  pure = Val
  Val f <*> Val x = Val $ f x
  Val f <*> E u q = E u (q |> (Val . f))
  E u q <*> Val x = E u (q |> (Val . ($ x)))
  E u q <*> m     = E u (q |> (`fmap` m))

instance Monad (Eff r) where
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}
  return = Val
  Val x >>= k = k x
  E u q >>= k = E u (q |> k)

-- | send a request and wait for a reply
send :: Member t r => t v -> Eff r v
send t = E (inj t) (tsingleton Val)

--------------------------------------------------------------------------------
                       -- Base Effect Runner --
--------------------------------------------------------------------------------
-- | Runs a set of Effects. Requires that all effects are consumed.
-- Typically composed as follows:
-- > run . runEff1 eff1Arg . runEff2 eff2Arg1 eff2Arg2 (program)
run :: Eff '[] w -> w
run (Val x) = x
run _       = error "Internal:run - This (E) should never happen"

-- the other case is unreachable since Union [] a cannot be
-- constructed. Therefore, run is a total function if its argument
-- terminates.

-- | Given a request, either handle it or relay it.
handleRelay :: (a -> Eff r w) ->
               (forall v. t v -> Arr r v w -> Eff r w) ->
               Eff (t ': r) a -> Eff r w
handleRelay ret h = loop
 where
  loop (Val x)  = ret x
  loop (E u' q)  = case decomp u' of
    Right x -> h x k
    Left  u -> E u (tsingleton k)
   where k = qComp q loop

-- | Parameterized 'handleRelay'
-- Allows sending along some state to be handled for the target
-- effect, or relayed to a handler that can handle the target effect.
handleRelayS :: s ->
                (s -> a -> Eff r w) ->
                (forall v. s -> t v -> (s -> Arr r v w) -> Eff r w) ->
                Eff (t ': r) a -> Eff r w
handleRelayS s' ret h = loop s'
  where
    loop s (Val x)  = ret s x
    loop s (E u' q)  = case decomp u' of
      Right x -> h s x k
      Left  u -> E u (tsingleton (k s))
     where k s'' x = loop s'' $ qApp q x

-- | Intercept the request and possibly reply to it, but leave it
-- unhandled
interpose :: Member t r =>
             (a -> Eff r w) -> (forall v. t v -> Arr r v w -> Eff r w) ->
             Eff r a -> Eff r w
interpose ret h = loop
 where
   loop (Val x)  = ret x
   loop (E u q)  = case prj u of
     Just x -> h x k
     _      -> E u (tsingleton k)
    where k = qComp q loop

--------------------------------------------------------------------------------
                    -- Nondeterministic Choice --
--------------------------------------------------------------------------------
-- | A data type for representing nondeterminstic choice
data NonDetEff a where
  MZero :: NonDetEff a     -- ^ the base choice
  MPlus :: NonDetEff Bool  -- ^ if True then choose left else choose right

instance Member NonDetEff r => Alternative (Eff r) where
  empty = mzero
  (<|>) = mplus

instance Member NonDetEff r => MonadPlus (Eff r) where
  mzero       = send MZero
  mplus m1 m2 = send MPlus >>= \x -> if x then m1 else m2

-- | A handler for nondeterminstic effects
makeChoiceA :: Alternative f
            => Eff (NonDetEff ': r) a -> Eff r (f a)
makeChoiceA =
  handleRelay (return . pure) $ \m k ->
    case m of
      MZero -> return empty
      MPlus -> liftM2 (<|>) (k True) (k False)

msplit :: Member NonDetEff r
       => Eff r a -> Eff r (Maybe (a, Eff r a))
msplit = loop []
  where loop jq (Val x)     = return (Just (x, msum jq))
        loop jq (E u q) =
          case prj u of
            Just MZero ->
              case jq of
                []     -> return Nothing
                (j:jq') -> loop jq' j
            Just MPlus -> loop (qApp q False : jq) (qApp q True)
            Nothing    -> E u (tsingleton k)
              where k = qComp q (loop jq)
