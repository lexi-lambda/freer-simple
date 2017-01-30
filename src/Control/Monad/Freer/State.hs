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
-- Portability:  POSIX
--
-- Composable handler for 'State' effects.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.State (
  State(..),
  get,
  put,
  modify,
  runState,
  evalState,
  execState,

  transactionState
) where

import Control.Monad ((>>), (>>=), return)
import Data.Either (Either(Left, Right))
import Data.Functor ((<$>), fmap)
import Data.Maybe (Maybe(Just))
import Data.Proxy (Proxy)
import Data.Tuple (fst, snd)

import Control.Monad.Freer.Internal


--------------------------------------------------------------------------------
                         -- State, strict --
--------------------------------------------------------------------------------

-- | Strict State effects: one can either Get values or Put them
data State s v where
  Get :: State s s
  Put :: !s -> State s ()

-- | Retrieve state
get :: Member (State s) r => Eff r s
get = send Get

-- | Store state
put :: Member (State s) r => s -> Eff r ()
put s = send (Put s)

-- | Modify state
modify :: Member (State s) r => (s -> s) -> Eff r ()
modify f = fmap f get >>= put

-- | Handler for State effects
runState :: Eff (State s ': r) w -> s -> Eff r (w,s)
runState (Val x) s = return (x,s)
runState (E u q) s = case decomp u of
  Right Get      -> runState (qApp q s) s
  Right (Put s') -> runState (qApp q ()) s'
  Left  u'       -> E u' (tsingleton (\x -> runState (qApp q x) s))

-- | Run a State effect, returning only the final state
execState :: Eff (State s ': r) w -> s -> Eff r s
execState st s = snd <$> runState st s

-- | Run a State effect, discarding the final state
evalState :: Eff (State s ': r) w -> s -> Eff r w
evalState st s = fst <$> runState st s


-- |
-- An encapsulated State handler, for transactional semantics
-- The global state is updated only if the transactionState finished
-- successfully
transactionState :: forall s r w. Member (State s) r =>
                    Proxy s -> Eff r w -> Eff r w
transactionState _ m = do s <- get; loop s m
 where
   loop :: s -> Eff r w -> Eff r w
   loop s (Val x) = put s >> return x
   loop s (E (u :: Union r b) q) = case prj u :: Maybe (State s b) of
     Just Get      -> loop s (qApp q s)
     Just (Put s') -> loop s'(qApp q ())
     _             -> E u (tsingleton k) where k = qComp q (loop s)
