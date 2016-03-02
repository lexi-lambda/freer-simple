{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Control.Monad.Freer.State
Description : State effects, for state-carrying computations.
Copyright   : Alej Cabrera 2015
License     : BSD-3
Maintainer  : cpp.cabrera@gmail.com
Stability   : experimental
Portability : POSIX

Composable handler for State effects.

Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a
starting point.

-}
module Control.Monad.Freer.State (
  State,
  get,
  put,
  modify,
  runState,

  ProxyState(..),
  transactionState
) where

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


data ProxyState s = ProxyState

-- |
-- An encapsulated State handler, for transactional semantics
-- The global state is updated only if the transactionState finished
-- successfully
transactionState :: forall s r w. Member (State s) r =>
                    ProxyState s -> Eff r w -> Eff r w
transactionState _ m = do s <- get; loop s m
 where
   loop :: s -> Eff r w -> Eff r w
   loop s (Val x) = put s >> return x
   loop s (E (u :: Union r b) q) = case prj u :: Maybe (State s b) of
     Just Get      -> loop s (qApp q s)
     Just (Put s') -> loop s'(qApp q ())
     _             -> E u (tsingleton k) where k = qComp q (loop s)
