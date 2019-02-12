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
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.FTCQueue
import Data.OpenUnion

-- | Effectful arrow type: a function from @a :: *@ to @b :: *@ that also does
-- effects denoted by @effs :: [* -> *]@.
type Arr effs a b = a -> Eff effs b

-- | An effectful function from @a :: *@ to @b :: *@ that is a composition of
-- several effectful functions. The paremeter @effs :: [* -> *]@ describes the
-- overall effect. The composition members are accumulated in a type-aligned
-- queue.
type Arrs effs a b = FTCQueue (Eff effs) a b

-- | The 'Eff' monad provides the implementation of a computation that performs
-- an arbitrary set of algebraic effects. In @'Eff' effs a@, @effs@ is a
-- type-level list that contains all the effects that the computation may
-- perform. For example, a computation that produces an 'Integer' by consuming a
-- 'String' from the global environment and acting upon a single mutable cell
-- containing a 'Bool' would have the following type:
--
-- @
-- 'Eff' '['Control.Monad.Freer.Reader.Reader' 'String', 'Control.Monad.Freer.State.State' 'Bool'] 'Integer'
-- @
--
-- Normally, a concrete list of effects is not used to parameterize 'Eff'.
-- Instead, the 'Member' or 'Members' constraints are used to express
-- constraints on the list of effects without coupling a computation to a
-- concrete list of effects. For example, the above example would more commonly
-- be expressed with the following type:
--
-- @
-- 'Members' '['Control.Monad.Freer.Reader.Reader' 'String', 'Control.Monad.Freer.State.State' 'Bool'] effs => 'Eff' effs 'Integer'
-- @
--
-- This abstraction allows the computation to be used in functions that may
-- perform other effects, and it also allows the effects to be handled in any
-- order.
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
{-# INLINE qApp #-}

-- | Composition of effectful arrows ('Arrs'). Allows for the caller to change
-- the effect environment, as well.
qComp :: Arrs effs a b -> (Eff effs b -> Eff effs' c) -> Arr effs' a c
qComp g h a = h $ qApp g a
{-# INLINE qComp #-}

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

instance (MonadIO m, LastMember m effs) => MonadIO (Eff effs) where
  liftIO = sendM . liftIO
  {-# INLINE liftIO #-}

-- | “Sends” an effect, which should be a value defined as part of an effect
-- algebra (see the module documentation for "Control.Monad.Freer"), to an
-- effectful computation. This is used to connect the definition of an effect to
-- the 'Eff' monad so that it can be used and handled.
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

-- | Runs a pure 'Eff' computation, since an 'Eff' computation that performs no
-- effects (i.e. has no effects in its type-level list) is guaranteed to be
-- pure. This is usually used as the final step of running an effectful
-- computation, after all other effects have been discharged using effect
-- handlers.
--
-- Typically, this function is composed as follows:
--
-- @
-- someProgram
--   'Data.Function.&' runEff1 eff1Arg
--   'Data.Function.&' runEff2 eff2Arg1 eff2Arg2
--   'Data.Function.&' 'run'
-- @
run :: Eff '[] a -> a
run (Val x) = x
run _       = error "Internal:run - This (E) should never happen"
{-# INLINE run #-}

-- | Like 'run', 'runM' runs an 'Eff' computation and extracts the result.
-- /Unlike/ 'run', 'runM' allows a single effect to remain within the type-level
-- list, which must be a monad. The value returned is a computation in that
-- monad, which is useful in conjunction with 'sendM' or 'liftBase' for plugging
-- in traditional transformer stacks.
runM :: Monad m => Eff '[m] a -> m a
runM (Val x) = return x
runM (E u q) = case extract u of
  mb -> mb >>= runM . qApp q
  -- The other case is unreachable since Union [] a cannot be constructed.
  -- Therefore, run is a total function if its argument terminates.
{-# INLINE runM #-}

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
