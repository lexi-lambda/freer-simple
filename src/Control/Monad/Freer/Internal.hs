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


-- The framework of extensible effects

-- ------------------------------------------------------------------------
-- A monadic library for communication between a handler and
-- its client, the administered computation

-- Effectful arrow type: a function from a to b that also does effects
-- denoted by r
type Arr r a b = a -> Eff r b

-- An effectful function from 'a' to 'b' that is a composition
-- of several effectful functions. The paremeter r describes the overall
-- effect.
-- The composition members are accumulated in a type-aligned queue
type Arrs r a b = FTCQueue (Eff r) a b

-- The Eff monad (not a transformer!)
-- It is a fairly standard coroutine monad
-- It is NOT a Free monad! There are no Functor constraints
-- Status of a coroutine (client): done with the value of type w,
-- or sending a request of type Union r with the continuation
-- Arrs r b a.
data Eff r a = Val a
             | forall b. E (Union r b) (Arrs r b a)

-- Application to the `generalized effectful function' Arrs r b w
qApp :: Arrs r b w -> b -> Eff r w
qApp q' x =
   case tviewl q' of
   TOne k  -> k x
   k :| t -> case k x of
     Val y -> qApp t y
     E u q -> E u (q >< t)

-- Compose effectful arrows (and possibly change the effect!)
qComp :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arr r' a c
qComp g h a = h $ qApp g a

-- Eff is still a monad and a functor (and Applicative)
-- (despite the lack of the Functor constraint)

instance Functor (Eff r) where
  {-# INLINE fmap #-}
  fmap f (Val x) = Val (f x)
  fmap f (E u q) = E u (q |> (Val . f)) -- does no mapping yet!

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
  E u q >>= k = E u (q |> k)          -- just accumulates continuations


-- send a request and wait for a reply
send :: Member t r => t v -> Eff r v
send t = E (inj t) (tsingleton Val)

--------------------------------------------------------------------------------
                       -- Base Effect Runner --
--------------------------------------------------------------------------------
-- The type of run ensures that all effects must be handled:
-- only pure computations may be run.
run :: Eff '[] w -> w
run (Val x) = x
run _       = error "Internal:run - This (E) should never happen"
-- the other case is unreachable since Union [] a cannot be
-- constructed.
-- Therefore, run is a total function if its argument terminates.

-- A convenient pattern: given a request (open union), either
-- handle it or relay it.
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

-- Parameterized handle_relay
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

-- Intercept the request and possibly reply to it, but leave it unhandled
-- (that's why we use the same r all throuout)
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
data NonDetEff a where
  MZero :: NonDetEff a
  MPlus :: NonDetEff Bool

instance Member NonDetEff r => Alternative (Eff r) where
  empty = mzero
  (<|>) = mplus

instance Member NonDetEff r => MonadPlus (Eff r) where
  mzero       = send MZero
  mplus m1 m2 = send MPlus >>= \x -> if x then m1 else m2

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
