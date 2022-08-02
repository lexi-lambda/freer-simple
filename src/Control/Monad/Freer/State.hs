{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module:       Control.Monad.Freer.State
-- Description:  State effects, for state-carrying computations.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King
-- License:      BSD3
-- Maintainer:   Alexis King <lexi.lambda@gmail.com>
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
  ( -- * State Effect
    State(..)

    -- * State Operations
  , get
  , put
  , modify
  , gets

    -- * State Handlers
  , runState
  , evalState
  , execState

    -- * State Utilities
  , transactionState
  , transactionState'
  , readonly
  , focus
  , appendonly
  , example
  ) where

import Data.Proxy (Proxy)

import Control.Monad.Freer (Eff, Member, send, type (~>), reinterpret, subsume)
import Control.Monad.Freer.Internal (Arr, handleRelayS, interposeS)
import Control.Monad.Freer.Reader (Reader(..), ask)
import Control.Monad.Freer.Writer (Writer(..), tell)
import Control.Lens

-- | Strict 'State' effects: one can either 'Get' values or 'Put' them.
data State s r where
  Get :: State s s
  Put :: !s -> State s ()

-- | Retrieve the current value of the state of type @s :: *@.
get :: forall s effs. Member (State s) effs => Eff effs s
get = send Get

-- | Set the current state to a specified value of type @s :: *@.
put :: forall s effs. Member (State s) effs => s -> Eff effs ()
put s = send (Put s)

-- | Modify the current state of type @s :: *@ using provided function
-- @(s -> s)@.
modify :: forall s effs. Member (State s) effs => (s -> s) -> Eff effs ()
modify f = fmap f get >>= put

-- | Retrieve a specific component of the current state using the provided
-- projection function.
gets :: forall s a effs. Member (State s) effs => (s -> a) -> Eff effs a
gets f = f <$> get

-- | Handler for 'State' effects.
runState :: forall s effs a. s -> Eff (State s ': effs) a -> Eff effs (a, s)
runState s0 = handleRelayS s0 (\s x -> pure (x, s)) $ \s x k -> case x of
  Get -> k s s
  Put s' -> k s' ()

-- | Run a 'State' effect, returning only the final state.
execState :: forall s effs a. s -> Eff (State s ': effs) a -> Eff effs s
execState s = fmap snd . runState s

-- | Run a State effect, discarding the final state.
evalState :: forall s effs a. s -> Eff (State s ': effs) a -> Eff effs a
evalState s = fmap fst . runState s

-- | An encapsulated State handler, for transactional semantics. The global
-- state is updated only if the 'transactionState' finished successfully.
--
-- GHC cannot infer the @s@ type parameter for this function, so it must be
-- specified explicitly with @TypeApplications@. Alternatively, it can be
-- specified by supplying a 'Proxy' to 'transactionState''.
transactionState
  :: forall s effs a
   . Member (State s) effs
  => Eff effs a
  -> Eff effs a
transactionState m = do
    s0 <- get @s
    (x, s) <- interposeS s0 (\s x -> pure (x, s)) handle m
    put s
    pure x
  where
    handle :: s -> State s v -> (s -> Arr effs v b) -> Eff effs b
    handle s x k = case x of
      Get -> k s s
      Put s' -> k s' ()

-- | Like 'transactionState', but @s@ is specified by providing a 'Proxy'
-- instead of requiring @TypeApplications@.
transactionState'
  :: forall s effs a
   . Member (State s) effs
  => Proxy s
  -> Eff effs a
  -> Eff effs a
transactionState' _ = transactionState @s
{-# INLINE transactionState' #-}

readonly
  :: forall s r effs
   . Lens' s r
  -> Eff (Reader r ': effs) ~> Eff (State s ': effs)
readonly l =
  reinterpret (\f -> do
    s <- get
    pure (readerAlg (view l s) f))
 where
  readerAlg :: r -> Reader r a -> a
  readerAlg x f = case f of Ask -> x

focus
  :: forall s s' effs
   . Lens' s s'
  -> Eff (State s' ': effs) ~> Eff (State s ': effs)
focus l =
  reinterpret (\f -> do
    s' <- get
    let (a, t) = stateAlg f (view l s')
    put (set l t s')
    pure a)
 where
  stateAlg :: State s' a -> s' -> (a, s')
  stateAlg f st =
    case f of
      Get   -> (st, st)
      Put s -> ((), s)

appendonly
  :: forall s w effs
   . Monoid w
  => Lens' s w
  -> Eff (Writer w ': effs) ~> Eff (State s ': effs)
appendonly l =
  reinterpret (\f -> do
    let (a, w) = writerAlg f
    modify (over l (mappend w))
    pure a)
 where
  writerAlg :: Writer w a -> (a, w)
  writerAlg f = case f of Tell x -> ((), x)

{- EXAMPLE OF USING THE ABOVE -}

data Foo = Foo
data Bar = Bar
data Baz = Baz

data SomeState = SomeState {
  _foo :: Foo,
  _bar :: Bar,
  _baz :: Baz
}

makeLenses ''SomeState

instance Semigroup Baz where
  Baz <> Baz = Baz

instance Monoid Baz where
  mappend = (<>)
  mempty = Baz

sample :: forall effs. Eff (Reader Foo ': State Bar ': Writer Baz ': effs) ()
sample = do
  Foo <- ask
  put Bar
  tell Baz

example :: forall effs. Eff (State SomeState ': effs) ()
example =
  subsume
    (focus baz
        (appendonly id
           (subsume
              (focus bar
                 (subsume
                    (focus foo
                       (readonly id sample)))))))
