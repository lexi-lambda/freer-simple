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

    -- ** Sending Arbitrary Effect
  , send
  , sendM

    -- ** Lifting Effect Stacks
  , raise

    -- * Handling Effects
  , Arr
  , run
  , runM

    -- ** Building Effect Handlers
  , interpret
  , reinterpret
  , reinterpret2
  , reinterpret3
  , reinterpretN
  , translate
  , interpretM
  ) where

import Control.Applicative (pure)
import Control.Monad (Monad, (>>=))
import Control.Natural (type (~>))
import Data.Function ((.))

import Control.Monad.Freer.Internal
  ( Arr
  , Eff
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
interpret f = handleRelay pure (\e -> (f e >>=))

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

-- | Like 'interpret', this function runs an effect without introducting another
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
