{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- Due to sendM.
{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- The following is needed to define MonadPlus instance. It is decidable
-- (there is no recursion!), but GHC cannot see that.
--
-- TODO: Remove once GHC can deduce the decidability of this instance.
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module:       Control.Monad.Freer.Internal
-- Description:  Mechanisms to make effects work.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King
-- License:      BSD3
-- Maintainer:   Alexis King <lexi.lambda@gmail.com>
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
  ( -- * Effect Monad
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
  , sendM

    -- ** Lifting Effect Stacks
  , raise

    -- * Handling Effects
  , run
  , runM

    -- ** Building Effect Handlers
  , handleRelay
  , handleRelayS
  , interpose
  , interposeS
  , replaceRelay
  , replaceRelayS
  , replaceRelayN

    -- *** Low-level Functions for Building Effect Handlers
  , qApp
  , qComp

    -- ** Nondeterminism Effect
  , NonDet(..)
  ) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.Base (MonadBase, liftBase)

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
  E u q <*> m     = E u (q |> (`fmap` m))
  {-# INLINE (<*>) #-}

instance Monad (Eff effs) where
  Val x >>= k = k x
  E u q >>= k = E u (q |> k)
  {-# INLINE (>>=) #-}

instance (MonadBase b m, LastMember m effs) => MonadBase b (Eff effs) where
  liftBase = sendM . liftBase
  {-# INLINE liftBase #-}

-- | Send a request and wait for a reply.
send :: Member eff effs => eff a -> Eff effs a
send t = E (inj t) (tsingleton Val)
{-# INLINE send #-}

-- | Identical to 'send', but specialized to the final effect in @effs@ to
-- assist type inference. This is useful for running actions in a monad
-- transformer stack used in conjunction with 'runM'.
sendM :: (Monad m, LastMember m effs) => m a -> Eff effs a
sendM = send
{-# INLINE sendM #-}

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

-- | Like 'replaceRelay', but with support for an explicit state to help
-- implement the interpreter.
replaceRelayS
  :: s
  -> (s -> a -> Eff (v ': effs) w)
  -> (forall x. s -> t x -> (s -> Arr (v ': effs) x w) -> Eff (v ': effs) w)
  -> Eff (t ': effs) a
  -> Eff (v ': effs) w
replaceRelayS s' pure' bind = loop s'
  where
    loop s (Val x)  = pure' s x
    loop s (E u' q) = case decomp u' of
        Right x -> bind s x k
        Left  u -> E (weaken u) (tsingleton (k s))
      where
        k s'' x = loop s'' $ qApp q x
{-# INLINE replaceRelayS #-}

-- | Interpret an effect by transforming it into another effect on top of the
-- stack. The primary use case of this function is allow interpreters to be
-- defined in terms of other ones without leaking intermediary implementation
-- details through the type signature.
replaceRelay
  :: (a -> Eff (v ': effs) w)
  -> (forall x. t x -> Arr (v ': effs) x w -> Eff (v ': effs) w)
  -> Eff (t ': effs) a
  -> Eff (v ': effs) w
replaceRelay pure' bind = loop
  where
    loop (Val x)  = pure' x
    loop (E u' q) = case decomp u' of
        Right x -> bind x k
        Left  u -> E (weaken u) (tsingleton k)
      where
        k = qComp q loop
{-# INLINE replaceRelay #-}

replaceRelayN
  :: forall gs t a effs w
   . Weakens gs
  => (a -> Eff (gs :++: effs) w)
  -> (forall x. t x -> Arr (gs :++: effs) x w -> Eff (gs :++: effs) w)
  -> Eff (t ': effs) a
  -> Eff (gs :++: effs) w
replaceRelayN pure' bind = loop
  where
    loop :: Eff (t ': effs) a -> Eff (gs :++: effs) w
    loop (Val x)  = pure' x
    loop (E u' (q :: Arrs (t ': effs) b a)) = case decomp u' of
        Right x -> bind x k
        Left  u -> E (weakens @gs u) (tsingleton k)
      where
        k :: Arr (gs :++: effs) b w
        k = qComp q loop
{-# INLINE replaceRelayN #-}

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
{-# INLINE handleRelay #-}

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
{-# INLINE handleRelayS #-}

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
{-# INLINE interpose #-}

-- | Like 'interpose', but with support for an explicit state to help implement
-- the interpreter.
interposeS
  :: Member eff effs
  => s
  -> (s -> a -> Eff effs b)
  -> (forall v. s -> eff v -> (s -> Arr effs v b) -> Eff effs b)
  -> Eff effs a
  -> Eff effs b
interposeS s' ret h = loop s'
  where
    loop s (Val x) = ret s x
    loop s (E u q) = case prj u of
        Just x -> h s x k
        _      -> E u (tsingleton (k s))
      where
        k s'' x = loop s'' $ qApp q x
{-# INLINE interposeS #-}

-- | Embeds a less-constrained 'Eff' into a more-constrained one. Analogous to
-- MTL's 'lift'.
raise :: Eff effs a -> Eff (e ': effs) a
raise = loop
  where
    loop (Val x) = pure x
    loop (E u q) = E (weaken u) . tsingleton $ qComp q loop
{-# INLINE raise #-}

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
