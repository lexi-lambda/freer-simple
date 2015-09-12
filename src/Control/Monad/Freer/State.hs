{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Freer.State (
  State,
  get,
  put,
  runState',
  runState,

  ProxyState(..),
  transactionState
) where

import Control.Monad.Freer.Internal

--------------------------------------------------------------------------------
                         -- State, strict --
--------------------------------------------------------------------------------

{- Initial design:
-- The state request carries with it the state mutator function
-- We can use this request both for mutating and getting the state.
-- But see below for a better design!
data State s v where
  State :: (s->s) -> State s s

In this old design, we have assumed that the dominant operation is
modify. Perhaps this is not wise. Often, the reader is most nominant.
-}
-- See also below, for decomposing the State into Reader and Writer!

-- The conventional design of State
data State s v where
  Get :: State s s
  Put :: !s -> State s ()

-- The signatures are inferred
get :: Member (State s) r => Eff r s
get = send Get

put :: Member (State s) r => s -> Eff r ()
put s = send (Put s)

runState' :: Eff (State s ': r) w -> s -> Eff r (w,s)
runState' m s' =
  handleRelayS s' (\s x -> return (x,s))
                  (\s sreq k -> case sreq of
                      Get    -> k s s
                      Put s'' -> k s'' ())
                  m

-- Since State is so frequently used, we optimize it a bit
runState :: Eff (State s ': r) w -> s -> Eff r (w,s)
runState (Val x) s = return (x,s)
runState (E u q) s = case decomp u of
  Right Get      -> runState (qApp q s) s
  Right (Put s') -> runState (qApp q ()) s'
  Left  u'       -> E u' (tsingleton (\x -> runState (qApp q x) s))


-- An encapsulated State handler, for transactional semantics
-- The global state is updated only if the transactionState finished
-- successfully
data ProxyState s = ProxyState
transactionState :: forall s r w. Member (State s) r =>
                    ProxyState s -> Eff r w -> Eff r w
transactionState _ m = do s <- get; loop s m
 where
   loop :: s -> Eff r w -> Eff r w
   loop s (Val x) = put s >> return x
   loop s (E (u :: Union r b) q) = case prj u :: Maybe (State s b) of
     Just Get      -> loop s (qApp q s)
     Just (Put s') -> loop s'(qApp q ())
     _      -> E u (tsingleton k) where k = qComp q (loop s)
