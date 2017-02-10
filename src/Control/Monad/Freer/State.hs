{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       Control.Monad.Freer.State
-- Description:  State effects, for state-carrying computations.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Composable handler for 'State' effects. Handy for passing an updatable state
-- through a computation.
--
-- Some computations may not require the full power of 'State' effect:
--
-- * For a read-only state, see "Control.Monad.Freer.Reader".
-- * To accumulate a value without using it on the way, see
--   "Control.Monad.Freer.Writer".
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.State
    (
    -- * State Effect
      State(..)

    -- * State Operations
    , get
    , put
    , modify

    -- * State Handlers
    , runState
    , evalState
    , execState

    -- * State Utilities
    , transactionState
    )
  where

import Control.Monad ((>>), (>>=), return)
import Data.Either (Either(Left, Right))
import Data.Functor ((<$>), fmap)
import Data.Maybe (Maybe(Just))
import Data.Proxy (Proxy)
import Data.Tuple (fst, snd)

import Control.Monad.Freer.Internal
    ( Eff(E, Val)
    , Member
    , Union
    , decomp
    , prj
    , qApp
    , qComp
    , send
    , tsingleton
    )


--------------------------------------------------------------------------------
                         -- State, strict --
--------------------------------------------------------------------------------

-- | Strict 'State' effects: one can either 'Get' values or 'Put' them.
data State s a where
    Get :: State s s
    Put :: !s -> State s ()

-- | Retrieve the current value of the state of type @s :: *@.
get :: Member (State s) effs => Eff effs s
get = send Get

-- | Set the current state to a specified value of type @s :: *@.
put :: Member (State s) effs => s -> Eff effs ()
put s = send (Put s)

-- | Modify the current state of type @s :: *@ using provided function
-- @(s -> s)@.
modify :: Member (State s) effs => (s -> s) -> Eff effs ()
modify f = fmap f get >>= put

-- | Handler for 'State' effects.
runState :: Eff (State s ': effs) a -> s -> Eff effs (a, s)
runState (Val x) s = return (x, s)
runState (E u q) s = case decomp u of
    Right Get      -> runState (qApp q s) s
    Right (Put s') -> runState (qApp q ()) s'
    Left  u'       -> E u' (tsingleton (\x -> runState (qApp q x) s))

-- | Run a 'State' effect, returning only the final state.
execState :: Eff (State s ': effs) a -> s -> Eff effs s
execState st s = snd <$> runState st s

-- | Run a State effect, discarding the final state.
evalState :: Eff (State s ': effs) a -> s -> Eff effs a
evalState st s = fst <$> runState st s

-- | An encapsulated State handler, for transactional semantics. The global
-- state is updated only if the 'transactionState' finished successfully.
transactionState
    :: forall s effs a
    .  Member (State s) effs
    => Proxy s
    -> Eff effs a
    -> Eff effs a
transactionState _ m = do s <- get; loop s m
  where
    loop :: s -> Eff effs a -> Eff effs a
    loop s (Val x)                = put s >> return x
    loop s (E (u :: Union r b) q) = case prj u :: Maybe (State s b) of
        Just Get      -> loop s (qApp q s)
        Just (Put s') -> loop s'(qApp q ())
        _             -> E u (tsingleton k) where k = qComp q (loop s)
