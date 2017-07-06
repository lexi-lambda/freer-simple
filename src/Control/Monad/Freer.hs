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

    -- ** Sending Arbitrary Effect
  , send

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
  ) where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Control.Natural (type (~>))

import Control.Monad.Freer.Internal
  ( Arr
  , Eff
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
