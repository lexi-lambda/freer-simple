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
    Freer (..)
  , usingFreer
  , type Eff

    -- ** Open Union
    --
    -- | Open Union (type-indexed co-product) of effects.
  , module Data.OpenUnion

    -- ** Natural Transformations
  , type (~>)

    -- ** Sending Arbitrary Effect
  , send
  , sendM

    -- ** Lifting Effect Stacks
  , raise
  , liftEff
  , hoistEff

    -- * Handling Effects
  , run
  , runM

    -- ** Nondeterminism Effect
  , NonDet(..)
  ) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Natural (type (~>))
import Data.Functor.Identity (Identity (..))

import Data.OpenUnion

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
type Eff r = Freer (Union r)

newtype Freer f a = Freer
  { runFreer :: forall m. Monad m => (f ~> m) -> m a
  }

instance Functor (Freer f) where
  fmap f (Freer m) = Freer $ \k -> fmap f $ m k
  {-# INLINE fmap #-}


instance Applicative (Freer f) where
  pure a = Freer $ const $ pure a
  {-# INLINE pure #-}

  Freer f <*> Freer a = Freer $ \k -> f k <*> a k
  {-# INLINE (<*>) #-}


instance Monad (Freer f) where
  return = pure
  {-# INLINE return #-}

  Freer ma >>= f = Freer $ \k -> do
    z <- ma k
    runFreer (f z) k
  {-# INLINE (>>=) #-}

instance (MonadBase b m, LastMember m effs) => MonadBase b (Eff effs) where
  liftBase = sendM . liftBase
  {-# INLINE liftBase #-}

instance (MonadIO m, LastMember m effs) => MonadIO (Eff effs) where
  liftIO = sendM . liftIO
  {-# INLINE liftIO #-}


instance MonadTrans Freer where
  lift = liftEff


instance MFunctor Freer where
  hoist = hoistEff


------------------------------------------------------------------------------
-- | Run a natural transformation over `Freer`.
hoistEff :: (f ~> g) -> Freer f ~> Freer g
hoistEff nat (Freer m) = Freer $ \k -> m $ k . nat
{-# INLINE hoistEff #-}


-- | Lift a value into 'Freer'. When 'f' is 'Union', this specializes as
-- @Union -- r x -> Eff r x@
liftEff :: f x -> Freer f x
liftEff u = Freer $ \k -> k u
{-# INLINE liftEff #-}


-- | “Sends” an effect, which should be a value defined as part of an effect
-- algebra (see the module documentation for "Control.Monad.Freer"), to an
-- effectful computation. This is used to connect the definition of an effect to
-- the 'Eff' monad so that it can be used and handled.
send :: Member eff effs => eff a -> Eff effs a
send = liftEff . inj
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
run :: Eff '[Identity] a -> a
run = runIdentity . runM
{-# INLINE run #-}

-- | Like 'run', 'runM' runs an 'Eff' computation and extracts the result.
-- /Unlike/ 'run', 'runM' allows a single effect to remain within the type-level
-- list, which must be a monad. The value returned is a computation in that
-- monad, which is useful in conjunction with 'sendM' or 'liftBase' for plugging
-- in traditional transformer stacks.
runM :: Monad m => Eff '[m] a -> m a
runM = usingFreer extract
{-# INLINE runM #-}

-- | @'flip' 'runFreer'@
usingFreer :: Monad m => (forall t. f t -> m t) -> Freer f a -> m a
usingFreer k m = runFreer m k

-- | Embeds a less-constrained 'Eff' into a more-constrained one. Analogous to
-- MTL's 'lift'.
raise :: Eff effs a -> Eff (e ': effs) a
raise = hoistEff weaken
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
