{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       Control.Monad.Freer
-- Description:  Freer - an extensible effects library
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
module Control.Monad.Freer
  ( -- * Effect Monad
    Eff

    -- ** Effect Constraints
  , Member
  , Members
  , LastMember

    -- ** Sending Arbitrary Effects
  , send
  , sendM

    -- ** Lifting Effect Stacks
  , raise

    -- * Handling Effects
  , run
  , runM

    -- ** Building Effect Handlers
    -- *** Basic effect handlers
  , interpret
  , interpose
    -- *** Derived effect handlers
  , reinterpret
  , reinterpret2
  , reinterpret3
  , reinterpretN
  , translate
    -- *** Monadic effect handlers
  , interpretM
    -- *** Advanced effect handlers
  , interpretWith
  , interposeWith
  ) where

import qualified Control.Monad.Freer.Internal as Internal

import Control.Applicative (pure)
import Control.Monad (Monad, (>>=))
import Control.Natural (type (~>))
import Data.Function ((.))

import Control.Monad.Freer.Internal
  ( Eff
  , LastMember
  , Member
  , Members
  , Weakens
  , (:++:)
  , handleRelay
  , raise
  , replaceRelay
  , replaceRelayN
  , run
  , runM
  , send
  , sendM
  )

-- | The simplest way to produce an effect handler. Given a natural
-- transformation from some effect @eff@ to some effectful computation with
-- effects @effs@, produces a natural transformation from @'Eff' (eff ': effs)@
-- to @'Eff' effs@.
interpret :: (eff ~> Eff effs) -> Eff (eff ': effs) ~> Eff effs
interpret f = interpretWith (\e -> (f e >>=))

-- | Like 'interpret', but instead of handling the effect, allows responding to
-- the effect while leaving it unhandled.
interpose :: Member eff effs => (eff ~> Eff effs) -> Eff effs ~> Eff effs
interpose f = interposeWith (\e -> (f e >>=))

-- | Like 'interpret', but instead of removing the interpreted effect @f@,
-- reencodes it in some new effect @g@.
reinterpret :: (f ~> Eff (g ': effs)) -> Eff (f ': effs) ~> Eff (g ': effs)
reinterpret f = replaceRelay pure (\e -> (f e >>=))

-- | Like 'reinterpret', but encodes the @f@ effect in /two/ new effects instead
-- of just one.
reinterpret2
  :: forall f g h effs
   . (f ~> Eff (g ': h ': effs)) -> Eff (f ': effs) ~> Eff (g ': h ': effs)
reinterpret2 = reinterpretN @[g, h]

-- | Like 'reinterpret', but encodes the @f@ effect in /three/ new effects
-- instead of just one.
reinterpret3
  :: forall f g h i effs
   . (f ~> Eff (g ': h ': i ': effs))
  -> Eff (f ': effs) ~> Eff (g ': h ': i ': effs)
reinterpret3 = reinterpretN @[g, h, i]

-- | Like 'interpret', 'reinterpret', 'reinterpret2', and 'reinterpret3', but
-- allows the result to have any number of additional effects instead of simply
-- 0-3. The problem is that this completely breaks type inference, so you will
-- have to explicitly pick @gs@ using @TypeApplications@. Prefer 'interpret',
-- 'reinterpret', 'reinterpret2', or 'reinterpret3' where possible.
reinterpretN
  :: forall gs f effs. Weakens gs
  => (f ~> Eff (gs :++: effs)) -> Eff (f ': effs) ~> Eff (gs :++: effs)
reinterpretN f = replaceRelayN @gs pure (\e -> (f e >>=))

-- | Runs an effect by translating it into another effect. This is effectively a
-- more restricted form of 'reinterpret', since both produce a natural
-- transformation from @'Eff' (f ': effs)@ to @'Eff' (g ': effs)@ for some
-- effects @f@ and @g@, but 'translate' does not permit using any of the other
-- effects in the implementation of the interpreter.
--
-- In practice, this difference in functionality is not particularly useful, and
-- 'reinterpret' easily subsumes all of the functionality of 'translate', but
-- the way 'translate' restricts the result leads to much better type inference.
--
-- @
-- 'translate' f = 'reinterpret' ('send' . f)
-- @
translate :: (f ~> g) -> Eff (f ': effs) ~> Eff (g ': effs)
translate f = reinterpret (send . f)

-- | Like 'interpret', this function runs an effect without introducing another
-- one. Like 'translate', this function runs an effect by translating it into
-- another effect in isolation, without access to the other effects in @effs@.
-- Unlike either of those functions, however, this runs the effect in a final
-- monad in @effs@, intended to be run with 'runM'.
--
-- @
-- 'interpretM' f = 'interpret' ('sendM' . f)
-- @
interpretM
  :: (Monad m, LastMember m effs)
  => (eff ~> m) -> Eff (eff ': effs) ~> Eff effs
interpretM f = interpret (sendM . f)

-- | A highly general way of handling an effect. Like 'interpret', but
-- explicitly passes the /continuation/, a function of type @v -> 'Eff' effs b@,
-- to the handler function. Most handlers invoke this continuation to resume the
-- computation with a particular value as the result, but some handlers may
-- return a value without resumption, effectively aborting the computation to
-- the point where the handler is invoked. This is useful for implementing
-- things like 'Control.Monad.Freer.Error.catchError', for example.
--
-- @
-- 'interpret' f = 'interpretWith' (\e -> (f e '>>='))
-- @
interpretWith
  :: (forall v. eff v -> (v -> Eff effs b) -> Eff effs b)
  -> Eff (eff ': effs) b
  -> Eff effs b
interpretWith = handleRelay pure

-- | Combines the interposition behavior of 'interpose' with the
-- continuation-passing capabilities of 'interpretWith'.
--
-- @
-- 'interpose' f = 'interposeWith' (\e -> (f e '>>='))
-- @
interposeWith
  :: Member eff effs
  => (forall v. eff v -> (v -> Eff effs b) -> Eff effs b)
  -> Eff effs b
  -> Eff effs b
interposeWith = Internal.interpose pure
