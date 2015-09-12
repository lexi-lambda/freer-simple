{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- The following is needed to define MonadPlus instance. It is decidable
-- (there is no recursion!), but GHC cannot see that.
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Freer.Internal (
  Eff(..),
  Member(..),
  Arr,
  Arrs,
  Union,

  decomp,
  tsingleton,

  qApp,
  qComp,
  send,
  run,
  handleRelay,
  handleRelayS,
  interpose,
) where

import Data.Open.Union
import Data.FTCQueue


-- The framework of extensible effects

-- ------------------------------------------------------------------------
-- A monadic library for communication between a handler and
-- its client, the administered computation

-- Effectful arrow type: a function from a to b that also does effects
-- denoted by r
type Arr r a b = a -> Eff r b

-- An effectful function from 'a' to 'b' that is a composition
-- of several effectful functions. The paremeter r describes the overall
-- effect.
-- The composition members are accumulated in a type-aligned queue
type Arrs r a b = FTCQueue (Eff r) a b

-- The Eff monad (not a transformer!)
-- It is a fairly standard coroutine monad
-- It is NOT a Free monad! There are no Functor constraints
-- Status of a coroutine (client): done with the value of type w,
-- or sending a request of type Union r with the continuation
-- Arrs r b a.
data Eff r a = Val a
             | forall b. E (Union r b) (Arrs r b a)

-- Application to the `generalized effectful function' Arrs r b w
qApp :: Arrs r b w -> b -> Eff r w
qApp q' x =
   case tviewl q' of
   TOne k  -> k x
   k :| t -> case k x of
     Val y -> qApp t y
     E u q -> E u (q >< t)

-- Compose effectful arrows (and possibly change the effect!)
qComp :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arr r' a c
qComp g h a = h $ qApp g a

-- Eff is still a monad and a functor (and Applicative)
-- (despite the lack of the Functor constraint)

instance Functor (Eff r) where
  {-# INLINE fmap #-}
  fmap f (Val x) = Val (f x)
  fmap f (E u q) = E u (q |> (Val . f)) -- does no mapping yet!

instance Applicative (Eff r) where
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}
  pure = Val
  Val f <*> Val x = Val $ f x
  Val f <*> E u q = E u (q |> (Val . f))
  E u q <*> Val x = E u (q |> (Val . ($ x)))
  E u q <*> m     = E u (q |> (`fmap` m))

instance Monad (Eff r) where
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}
  return = Val
  Val x >>= k = k x
  E u q >>= k = E u (q |> k)          -- just accumulates continuations


-- send a request and wait for a reply
send :: Member t r => t v -> Eff r v
send t = E (inj t) (tsingleton Val)

-- ------------------------------------------------------------------------
-- The initial case, no effects

-- The type of run ensures that all effects must be handled:
-- only pure computations may be run.
run :: Eff '[] w -> w
run (Val x) = x
run _       = error "Internal:run - This (E) should never happen"
-- the other case is unreachable since Union [] a cannot be
-- constructed.
-- Therefore, run is a total function if its argument terminates.

-- A convenient pattern: given a request (open union), either
-- handle it or relay it.
handleRelay :: (a -> Eff r w) ->
               (forall v. t v -> Arr r v w -> Eff r w) ->
               Eff (t ': r) a -> Eff r w
handleRelay ret h = loop
 where
  loop (Val x)  = ret x
  loop (E u' q)  = case decomp u' of
    Right x -> h x k
    Left  u -> E u (tsingleton k)
   where k = qComp q loop

-- Parameterized handle_relay
handleRelayS :: s ->
                (s -> a -> Eff r w) ->
                (forall v. s -> t v -> (s -> Arr r v w) -> Eff r w) ->
                Eff (t ': r) a -> Eff r w
handleRelayS s' ret h = loop s'
  where
    loop s (Val x)  = ret s x
    loop s (E u' q)  = case decomp u' of
      Right x -> h s x k
      Left  u -> E u (tsingleton (k s))
     where k s'' x = loop s'' $ qApp q x

-- Add something like Control.Exception.catches? It could be useful
-- for control with cut.

-- Intercept the request and possibly reply to it, but leave it unhandled
-- (that's why we use the same r all throuout)
interpose :: Member t r =>
             (a -> Eff r w) -> (forall v. t v -> Arr r v w -> Eff r w) ->
             Eff r a -> Eff r w
interpose ret h = loop
 where
   loop (Val x)  = ret x
   loop (E u q)  = case prj u of
     Just x -> h x k
     _      -> E u (tsingleton k)
    where k = qComp q loop

{- EXAMPLE
rdwr :: (Member (Reader Int) r, Member (Writer String) r)
     => Eff r Int
rdwr = do
  tell "begin"
  r <- addN 10
  tell "end"
  return r

rdwrr :: (Int,[String])
rdwrr = run . (`runReader` (1::Int)) . runWriter $ rdwr
-- (10,["begin","end"])
-}

-- Encapsulation of effects
-- The example suggested by a reviewer

{- The reviewer outlined an MTL implementation below, writing
  ``This hides the state effect and I can layer another state effect on
  top without getting into conflict with the class system.''

class Monad m => MonadFresh m where
    fresh :: m Int

newtype FreshT m a = FreshT { unFreshT :: State Int m a }
      deriving (Functor, Monad, MonadTrans)

    instance Monad m => MonadFresh (FreshT m) where
      fresh = FreshT $ do n <- get; put (n+1); return n

See EncapsMTL.hs for the complete code.
-}

-- There are three possible implementations
-- The first one uses State Fresh where
--    newtype Fresh = Fresh Int
-- We get the `private' effect layer (State Fresh) that does not interfere
-- with with other layers.
-- This is the easiest implementation.

-- The second implementation defines a new effect Fresh

{-
-- Finally, the worst implementation but the one that answers
-- reviewer's question: implementing Fresh in terms of State
-- but not revealing that fact.

runFresh :: Eff (Fresh :> r) w -> Int -> Eff r w
runFresh m s = runState m' s >>= return . fst
 where
 m' = loop m
 loop (Val x) = return x
 loop (E u q)   = case decomp u of
  Right Fresh -> do
                 n <- get
                 put (n+1::Int)
                 k n
  Left u  -> send (\k -> weaken $ fmap k u) >>= loop

tfresh = runTrace $ flip runFresh 0 $ do
  n <- fresh
  -- (x::Int) <- get
  trace $ "Fresh " ++ show n
  n <- fresh
  trace $ "Fresh " ++ show n

{-
If we try to meddle with the encapsulated state, by uncommenting the
get statement above, we get:
    No instance for (Member (State Int) Void)
      arising from a use of `get'
-}

-}

-- ------------------------------------------------------------------------
-- Lifting: emulating monad transformers

-- newtype Lift m a = Lift (m a)


{-
 -- ------------------------------------------------------------------------
-- Co-routines
-- The interface is intentionally chosen to be the same as in transf.hs

-- The yield request: reporting the value of type a and suspending
-- the coroutine. Resuming with the value of type b
data Yield a b v = Yield a (b -> v)
    deriving (Typeable, Functor)

-- The signature is inferred
yield :: (Typeable a, Typeable b, Member (Yield a b) r) => a -> Eff r b
yield x = send (inj . Yield x)

-- Status of a thread: done or reporting the value of the type a
-- and resuming with the value of type b
data Y r a b = Done | Y a (b -> Eff r (Y r a b))

-- Launch a thread and report its status
runC :: (Typeable a, Typeable b) =>
        Eff (Yield a b :> r) w -> Eff r (Y r a b)
runC m = loop (admin m) where
 loop (Val x) = return Done
 loop (E u)   = handle_relay u loop $
                 \(Yield x k) -> return (Y x (loop . k))


-- First example of coroutines
yieldInt :: Member (Yield Int ()) r => Int -> Eff r ()
yieldInt = yield

th1 :: Member (Yield Int ()) r => Eff r ()
th1 = yieldInt 1 >> yieldInt 2


c1 = runTrace (loop =<< runC th1)
 where loop (Y x k) = trace (show (x::Int)) >> k () >>= loop
       loop Done    = trace "Done"
{-
1
2
Done
-}

-- Add dynamic variables
-- The code is essentially the same as that in transf.hs (only added
-- a type specializtion on yield). The inferred signature is different though.
-- Before it was
--    th2 :: MonadReader Int m => CoT Int m ()
-- Now it is more general:
th2 :: (Member (Yield Int ()) r, Member (Reader Int) r) => Eff r ()
th2 = ask >>= yieldInt >> (ask >>= yieldInt)


-- Code is essentially the same as in transf.hs; no liftIO though
c2 = runTrace $ runReader (loop =<< runC th2) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> k () >>= loop
       loop Done    = trace "Done"
{-
10
10
Done
-}

-- locally changing the dynamic environment for the suspension
c21 = runTrace $ runReader (loop =<< runC th2) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
       loop Done    = trace "Done"
{-
10
11
Done
-}

-- Real example, with two sorts of local rebinding
th3 :: (Member (Yield Int ()) r, Member (Reader Int) r) => Eff r ()
th3 = ay >> ay >> local (+(10::Int)) (ay >> ay)
 where ay = ask >>= yieldInt

c3 = runTrace $ runReader (loop =<< runC th3) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> k () >>= loop
       loop Done    = trace "Done"
{-
10
10
20
20
Done
-}

-- locally changing the dynamic environment for the suspension
c31 = runTrace $ runReader (loop =<< runC th3) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
       loop Done    = trace "Done"
{-
10
11
21
21
Done
-}
-- The result is exactly as expected and desired: the coroutine shares the
-- dynamic environment with its parent; however, when the environment
-- is locally rebound, it becomes private to coroutine.

-- We now make explicit that the client computation, run by th4,
-- is abstract. We abstract it out of th4
c4 = runTrace $ runReader (loop =<< runC (th4 client)) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
       loop Done    = trace "Done"

       -- cl, client, ay are monomorphic bindings
       th4 cl = cl >> local (+(10::Int)) cl
       client = ay >> ay
       ay     = ask >>= yieldInt

{-
10
11
21
21
Done
-}

-- Even more dynamic example
c5 = runTrace $ runReader (loop =<< runC (th client)) (10::Int)
 where loop (Y x k) = trace (show (x::Int)) >> local (\y->x+1) (k ()) >>= loop
       loop Done    = trace "Done"

       -- cl, client, ay are monomorphic bindings
       client = ay >> ay >> ay
       ay     = ask >>= yieldInt

       -- There is no polymorphic recursion here
       th cl = do
         cl
         v <- ask
         (if v > (20::Int) then id else local (+(5::Int))) cl
         if v > (20::Int) then return () else local (+(10::Int)) (th cl)
{-
10
11
12
18
18
18
29
29
29
29
29
29
Done
-}

-- And even more
c7 = runTrace $
      runReader (runReader (loop =<< runC (th client)) (10::Int)) (1000::Double)
 where loop (Y x k) = trace (show (x::Int)) >>
                      local (\y->fromIntegral (x+1)::Double) (k ()) >>= loop
       loop Done    = trace "Done"

       -- cl, client, ay are monomorphic bindings
       client = ay >> ay >> ay
       ay     = ask >>= \x -> ask >>=
                 \y -> yieldInt (x + round (y::Double))

       -- There is no polymorphic recursion here
       th cl = do
         cl
         v <- ask
         (if v > (20::Int) then id else local (+(5::Int))) cl
         if v > (20::Int) then return () else local (+(10::Int)) (th cl)

{-
1010
1021
1032
1048
1064
1080
1101
1122
1143
1169
1195
1221
1252
1283
1314
1345
1376
1407
Done
-}

c7' = runTrace $
      runReader (runReader (loop =<< runC (th client)) (10::Int)) (1000::Double)
 where loop (Y x k) = trace (show (x::Int)) >>
                      local (\y->fromIntegral (x+1)::Double) (k ()) >>= loop
       loop Done    = trace "Done"

       -- cl, client, ay are monomorphic bindings
       client = ay >> ay >> ay
       ay     = ask >>= \x -> ask >>=
                 \y -> yieldInt (x + round (y::Double))

       -- There is no polymorphic recursion here
       th cl = do
         cl
         v <- ask
         (if v > (20::Int) then id else local (+(5::Double))) cl
         if v > (20::Int) then return () else local (+(10::Int)) (th cl)
{-
1010
1021
1032
1048
1048
1048
1069
1090
1111
1137
1137
1137
1168
1199
1230
1261
1292
1323
Done
-}

-- ------------------------------------------------------------------------
-- An example of non-trivial interaction of effects, handling of two
-- effects together
-- Non-determinism with control (cut)
-- For the explanation of cut, see Section 5 of Hinze ICFP 2000 paper.
-- Hinze suggests expressing cut in terms of cutfalse
--  ! = return () `mplus` cutfalse
-- where
--  cutfalse :: m a
-- satisfies the following laws
--   cutfalse >>= k  = cutfalse              (F1)
--   cutfalse | m    = cutfalse              (F2)
-- (note: m `mplus` cutfalse is different from cutfalse `mplus` m)
-- In other words, cutfalse is the left zero of both bind and mplus.
--
-- Hinze also introduces the operation call :: m a -> m a that
-- delimits the effect of cut: call m executes m. If the cut is
-- invoked in m, it discards only the choices made since m was called.
-- Hinze postulates the axioms of call:
--
--   call false = false                          (C1)
--   call (return a | m) = return a | call m     (C2)
--   call (m | cutfalse) = call m                (C3)
--   call (lift m >>= k) = lift m >>= (call . k) (C4)
--
-- call m behaves like m except any cut inside m has only a local effect,
-- he says.

-- Hinze noted a problem with the `mechanical' derivation of backtracing
-- monad transformer with cut: no axiom specifying the interaction of
-- call with bind; no way to simplify nested invocations of call.

-- We use exceptions for cutfalse
-- Therefore, the law ``cutfalse >>= k       = cutfalse''
-- is satisfied automatically since all exceptions have the above property.

data CutFalse = CutFalse deriving Typeable

cutfalse = throwError CutFalse

-- The interpreter -- it is like reify . reflect with a twist
-- Compare this implementation with the huge implementation of call
-- in Hinze 2000 (Figure 9)
-- Each clause corresponds to the axiom of call or cutfalse.
-- All axioms are covered.
-- The code clearly expresses the intuition that call watches the choice points
-- of its argument computation. When it encounteres a cutfalse request,
-- it discards the remaining choicepoints.

-- It completely handles CutFalse effects but not non-determinism
call :: Member Choose r => Eff (Exc CutFalse :> r) a -> Eff r a
call m = loop [] (admin m) where
 loop jq (Val x) = return x `mplus'` next jq          -- (C2)
 loop jq (E u) = case decomp u of
    Right (Exc CutFalse) -> mzero'  -- drop jq (F2)
    Left u -> check jq u

 check jq u | Just (Choose [] _) <- prj u  = next jq  -- (C1)
 check jq u | Just (Choose [x] k) <- prj u = loop jq (k x)  -- (C3), optim
 check jq u | Just (Choose lst k) <- prj u = next $ map k lst ++ jq -- (C3)
 check jq u = send (\k -> fmap k u) >>= loop jq      -- (C4)

 next []    = mzero'
 next (h:t) = loop t h

-- The signature is inferred
tcut1 :: (Member Choose r, Member (Exc CutFalse) r) => Eff r Int
tcut1 = (return (1::Int) `mplus'` return 2) `mplus'`
         ((cutfalse `mplus'` return 4) `mplus'`
          return 5)

tcut1r = run . makeChoice $ call tcut1
-- [1,2]

tcut2 = return (1::Int) `mplus'`
         call (return 2 `mplus'` (cutfalse `mplus'` return 3) `mplus'`
               return 4)
       `mplus'` return 5

-- Here we see nested call. It poses no problems...
tcut2r = run . makeChoice $ call tcut2
-- [1,2,5]

-- More nested calls
tcut3 = call tcut1 `mplus'` call (tcut2 `mplus'` cutfalse)
tcut3r = run . makeChoice $ call tcut3
-- [1,2,1,2,5]

tcut4 = call tcut1 `mplus'`  (tcut2 `mplus'` cutfalse)
tcut4r = run . makeChoice $ call tcut4
-- [1,2,1,2,5]
-}
