{-# LANGUAGE FlexibleContexts #-}
module Reader where

import Control.Applicative
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Writer

import Common

--------------------------------------------------------------------------------
                            -- Examples --
--------------------------------------------------------------------------------
t1 :: Member (Reader Int) r => Eff r Int
t1 = ask `add` return (1 :: Int)

t1' :: Member (Reader Int) r => Eff r Int
t1' = do v <- ask; return (v + 1 :: Int)

t1r :: Eff r Int
t1r = runReader t1 (10::Int)

t1rr :: Bool
t1rr = 11 == run t1r

{-
t1rr' = run t1
    No instance for (Member (Reader Int) Void)
      arising from a use of `t1'
-}

t2 :: (Member (Reader Int) r, Member (Reader Float) r) => Eff r Float
t2 = do
  v1 <- ask
  v2 <- ask
  return $ fromIntegral (v1 + (1::Int)) + (v2 + (2::Float))

t2r :: Member (Reader Float) r => Eff r Float
t2r = runReader t2 (10::Int)

t2rr :: Eff r Float
t2rr = flip runReader (20::Float) . flip runReader (10::Int) $ t2

t2rrr :: Bool
t2rrr = 33.0 == run t2rr

-- The opposite order of layers
{- If we mess up, we get an error
t2rrr1' = run $ runReader (runReader t2 (20::Float)) (10::Float)
    No instance for (Member (Reader Int) [])
      arising from a use of `t2'
-}
t2rrr' :: Bool
t2rrr' = (33.0 ==) $
         run $ runReader (runReader t2 (20 :: Float)) (10 :: Int)

t3 :: Member (Reader Int) r => Eff r Int
t3 = t1 `add` local (+ (10::Int)) t1

t3r :: Bool
t3r = (212 ==) $ run $ runReader t3 (100::Int)


-- The following example demonstrates true interleaving of Reader Int
-- and Reader Float layers
t4 :: (Member (Reader Int) r, Member (Reader Float) r) =>
      Eff r Float
t4 = liftA2 (+) (local (+ (10::Int)) t2)
                (local (+ (30::Float)) t2)

t4rr :: Bool
t4rr = (106.0 ==) $ run $ runReader (runReader t4 (10::Int)) (20::Float)

-- The opposite order of layers gives the same result
t4rr' :: Bool
t4rr' = (106.0 ==) $ run $ runReader (runReader t4 (20 :: Float)) (10 :: Int)

addGet :: Member (Reader Int) r => Int -> Eff r Int
addGet x = ask >>= \i -> return (i+x)

addN :: Member (Reader Int) r => Int -> Eff r Int
addN n = foldl (>>>) return (replicate n addGet) 0
 where f >>> g = (>>= g) . f

-- Map an effectful function
tmap :: Member (Reader Int) r => Eff r [Int]
tmap = mapM f [1..5]
 where f x = ask `add` return x

tmapr :: Bool
tmapr = ([11,12,13,14,15] ==) $
        run $ runReader tmap (10::Int)

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
