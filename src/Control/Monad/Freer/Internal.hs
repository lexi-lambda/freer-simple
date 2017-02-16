{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- The following is needed to define MonadPlus instance. It is decidable
-- (there is no recursion!), but GHC cannot see that.
--
-- TODO: Remove once GHC can deduce the decidability of this instance.
{-# LANGUAGE UndecidableInstances #-}

-- Due to re-export of Data.FTCQueue, and Data.OpenUnion.
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

-- |
-- Module:       Control.Monad.Freer.Internal
-- Description:  Mechanisms to make effects work.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Internal machinery for this effects library. This includes:
--
-- * 'Eff' data type, for expressing effects.
-- * 'NonDet' data type, for nondeterministic effects.
-- * Functions for facilitating the construction of effects and their handlers.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Internal
    (
    -- * Effect Monad
      Eff(..)
    , Arr
    , Arrs

    -- ** Open Union
    --
    -- | Open Union (type-indexed co-product) of effects.
    , module Data.OpenUnion

    -- ** Fast Type-aligned Queue
    --
    -- | Fast type-aligned queue optimized to effectful functions of type
    -- @(a -> m b)@.
    , module Data.FTCQueue

    -- ** Sending Arbitrary Effect
    , send

    -- * Handling Effects
    , run
    , runM

    -- ** Building Effect Handlers
    , handleRelay
    , handleRelayS
    , interpose

    -- *** Low-level Functions for Building Effect Handlers
    , qApp
    , qComp

    -- ** Nondeterminism Effect
    , NonDet(..)
    )
  where

import Prelude (error)  -- Function error is used for imposible cases.

import Control.Applicative
    ( Alternative((<|>), empty)
    , Applicative((<*>), pure)
    )
import Control.Monad
    ( Monad((>>=), return)
    , MonadPlus(mplus, mzero)
    )
import Data.Bool (Bool)
import Data.Either (Either(Left, Right))
import Data.Function (($), (.))
import Data.Functor (Functor(fmap))
import Data.Maybe (Maybe(Just))

import Data.FTCQueue
import Data.OpenUnion


-- | Effectful arrow type: a function from @a :: *@ to @b :: *@ that also does
-- effects denoted by @effs :: [* -> *]@.
type Arr effs a b = a -> Eff effs b

-- | An effectful function from @a :: *@ to @b :: *@ that is a composition of
-- several effectful functions. The paremeter @eff :: [* -> *]@ describes the
-- overall effect. The composition members are accumulated in a type-aligned
-- queue.
type Arrs effs a b = FTCQueue (Eff effs) a b

-- | The Eff monad provides a way to use effects in Haskell, in such a way that
-- different types of effects can be interleaved, and so that the produced code
-- is efficient.
data Eff effs a
    = Val a
    -- ^ Pure value (@'return' = 'pure' = 'Val'@).
    | forall b. E (Union effs b) (Arrs effs b a)
    -- ^ Sending a request of type @Union effs@ with the continuation
    -- @'Arrs' r b a@.

-- | Function application in the context of an array of effects,
-- @'Arrs' effs b w@.
qApp :: Arrs effs b w -> b -> Eff effs w
qApp q' x = case tviewl q' of
    TOne k  -> k x
    k :| t -> case k x of
        Val y -> qApp t y
        E u q -> E u (q >< t)

-- | Composition of effectful arrows ('Arrs'). Allows for the caller to change
-- the effect environment, as well.
qComp :: Arrs effs a b -> (Eff effs b -> Eff effs' c) -> Arr effs' a c
qComp g h a = h $ qApp g a

instance Functor (Eff effs) where
    fmap f (Val x) = Val (f x)
    fmap f (E u q) = E u (q |> (Val . f))
    {-# INLINE fmap #-}

instance Applicative (Eff effs) where
    pure = Val
    {-# INLINE pure #-}

    Val f <*> Val x = Val $ f x
    Val f <*> E u q = E u (q |> (Val . f))
    E u q <*> Val x = E u (q |> (Val . ($ x)))
    E u q <*> m     = E u (q |> (`fmap` m))
    {-# INLINE (<*>) #-}

instance Monad (Eff effs) where
    -- Future versions of GHC will consider any other definition as error.
    return = pure
    {-# INLINE return #-}

    Val x >>= k = k x
    E u q >>= k = E u (q |> k)
    {-# INLINE (>>=) #-}

-- | Send a request and wait for a reply.
send :: Member eff effs => eff a -> Eff effs a
send t = E (inj t) (tsingleton Val)

--------------------------------------------------------------------------------
                       -- Base Effect Runner --
--------------------------------------------------------------------------------

-- | Runs a set of Effects. Requires that all effects are consumed.
-- Typically composed as follows:
--
-- @
-- 'run' . runEff1 eff1Arg . runEff2 eff2Arg1 eff2Arg2 $ someProgram
-- @
run :: Eff '[] a -> a
run (Val x) = x
run _       = error "Internal:run - This (E) should never happen"

-- | Runs a set of Effects. Requires that all effects are consumed, except for
-- a single effect known to be a monad. The value returned is a computation in
-- that monad. This is useful for plugging in traditional transformer stacks.
runM :: Monad m => Eff '[m] a -> m a
runM (Val x) = return x
runM (E u q) = case extract u of
    mb -> mb >>= runM . qApp q
    -- The other case is unreachable since Union [] a cannot be constructed.
    -- Therefore, run is a total function if its argument terminates.

-- | Given a request, either handle it or relay it.
handleRelay
    :: (a -> Eff effs b)
    -- ^ Handle a pure value.
    -> (forall v. eff v -> Arr effs v b -> Eff effs b)
    -- ^ Handle a request for effect of type @eff :: * -> *@.
    -> Eff (eff ': effs) a
    -> Eff effs b
    -- ^ Result with effects of type @eff :: * -> *@ handled.
handleRelay ret h = loop
  where
    loop (Val x)  = ret x
    loop (E u' q) = case decomp u' of
        Right x -> h x k
        Left  u -> E u (tsingleton k)
      where
        k = qComp q loop

-- | Parameterized 'handleRelay'. Allows sending along some state of type
-- @s :: *@ to be handled for the target effect, or relayed to a handler that
-- can- handle the target effect.
handleRelayS
    :: s
    -> (s -> a -> Eff effs b)
    -- ^ Handle a pure value.
    -> (forall v. s -> eff v -> (s -> Arr effs v b) -> Eff effs b)
    -- ^ Handle a request for effect of type @eff :: * -> *@.
    -> Eff (eff ': effs) a
    -> Eff effs b
    -- ^ Result with effects of type @eff :: * -> *@ handled.
handleRelayS s' ret h = loop s'
  where
    loop s (Val x)  = ret s x
    loop s (E u' q) = case decomp u' of
        Right x -> h s x k
        Left  u -> E u (tsingleton (k s))
      where
        k s'' x = loop s'' $ qApp q x

-- | Intercept the request and possibly reply to it, but leave it unhandled.
interpose
    :: Member eff effs
    => (a -> Eff effs b)
    -> (forall v. eff v -> Arr effs v b -> Eff effs b)
    -> Eff effs a
    -> Eff effs b
interpose ret h = loop
  where
    loop (Val x) = ret x
    loop (E u q) = case prj u of
        Just x -> h x k
        _      -> E u (tsingleton k)
      where
        k = qComp q loop

--------------------------------------------------------------------------------
                    -- Nondeterministic Choice --
--------------------------------------------------------------------------------

-- | A data type for representing nondeterminstic choice.
data NonDet a where
    MZero :: NonDet a
    MPlus :: NonDet Bool

instance Member NonDet effs => Alternative (Eff effs) where
    empty = mzero
    (<|>) = mplus

instance Member NonDet effs => MonadPlus (Eff effs) where
    mzero       = send MZero
    mplus m1 m2 = send MPlus >>= \x -> if x then m1 else m2
