{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Control.Monad.Freer.Reader (
  Reader(..),

  ask,
  runReader',
  runReader,
  local
) where

import Control.Applicative
import Control.Monad.Freer.Internal

-- ------------------------------------------------------------------------
-- The Reader monad

-- The request for a value of type e from the current environment
-- This is a GADT because the type of values
-- returned in response to a (Reader e a) request is not any a;
-- we expect in reply the value of type 'e', the value from the
-- environment. So, the return type is restricted: 'a ~ e'
data Reader e v where
  Reader :: Reader e e

-- One can also define this as
--    data Reader e v = (e ~ v) => Reader
-- and even without GADTs, using explicit coercion:
--    newtype Reader e v = Reader (e->v)
-- In the latter case, when we make the request, we make it as Reader id.
-- So, strictly speaking, GADTs are not really necessary.


-- The signature is inferred
ask :: (Member (Reader e) r) => Eff r e
ask = send Reader

-- The handler of Reader requests. The return type shows that
-- all Reader requests are fully handled.
runReader' :: Eff (Reader e ': r) w -> e -> Eff r w
runReader' m e = loop m where
 loop (Val x) = return x
 loop (E u' q) = case decomp u' of
                  Right Reader -> loop $ qApp q e
                  Left  u      -> E u (tsingleton (qComp q loop))

-- A different way of writing the above
runReader :: Eff (Reader e ': r) w -> e -> Eff r w
runReader m e = handleRelay return (\Reader k -> k e) m

-- Locally rebind the value in the dynamic environment
-- This function is like a relay; it is both an admin for Reader requests,
-- and a requestor of them
local :: forall e a r. Member (Reader e) r =>
         (e -> e) -> Eff r a -> Eff r a
local f m = do
  e0 <- ask
  let e = f e0
  -- Local signature is needed, as always with GADTs
  let h :: Reader e v -> Arr r v a -> Eff r a
      h Reader g = g e
  interpose return h m


-- Examples
add :: Applicative f => f Int -> f Int -> f Int
add = liftA2 (+)


-- The type is inferred
t1 :: Member (Reader Int) r => Eff r Int
t1 = ask `add` return (1 :: Int)

t1' :: Member (Reader Int) r => Eff r Int
t1' = do v <- ask; return (v + 1 :: Int)

-- t1r :: Eff r Int
t1r = runReader t1 (10::Int)

t1rr = 11 == run t1r

{-
t1rr' = run t1
    No instance for (Member (Reader Int) Void)
      arising from a use of `t1'
-}

-- Inferred type
-- t2 :: (Member (Reader Int) r, Member (Reader Float) r) => Eff r Float
t2 = do
  v1 <- ask
  v2 <- ask
  return $ fromIntegral (v1 + (1::Int)) + (v2 + (2::Float))

-- t2r :: Member (Reader Float) r => Eff r Float
t2r = runReader t2 (10::Int)
-- t2rr :: Eff r Float
t2rr = flip runReader (20::Float) . flip runReader (10::Int) $ t2

t2rrr = 33.0 == run t2rr

-- The opposite order of layers
{- If we mess up, we get an error
t2rrr1' = run $ runReader (runReader t2 (20::Float)) (10::Float)
    No instance for (Member (Reader Int) [])
      arising from a use of `t2'
-}
t2rrr' = (33.0 ==) $
         run $ runReader (runReader t2 (20 :: Float)) (10 :: Int)

-- The type is inferred
t3 :: Member (Reader Int) r => Eff r Int
t3 = t1 `add` local (+ (10::Int)) t1
t3r = (212 ==) $ run $ runReader t3 (100::Int)


-- The following example demonstrates true interleaving of Reader Int
-- and Reader Float layers
{-
t4
  :: (Member (Reader Int) r, Member (Reader Float) r) =>
     () -> Eff r Float
-}
t4 = liftA2 (+) (local (+ (10::Int)) t2)
                (local (+ (30::Float)) t2)

t4rr = (106.0 ==) $ run $ runReader (runReader t4 (10::Int)) (20::Float)

-- The opposite order of layers gives the same result
t4rr' = (106.0 ==) $ run $ runReader (runReader t4 (20 :: Float)) (10 :: Int)

addGet :: Member (Reader Int) r => Int -> Eff r Int
addGet x = ask >>= \i -> return (i+x)

addN n = foldl (>>>) return (replicate n addGet) 0
 where f >>> g = (>>= g) . f

-- Map an effectful function
-- The type is inferred
tmap :: Member (Reader Int) r => Eff r [Int]
tmap = mapM f [1..5]
 where f x = ask `add` return x

tmapr = ([11,12,13,14,15] ==) $
        run $ runReader tmap (10::Int)
