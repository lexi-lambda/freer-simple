{-# LANGUAGE FlexibleContexts #-}
module Tests.Reader (
  testReader,
  testMultiReader,
  testLocal
) where

import Control.Monad.Freer
import Control.Monad.Freer.Reader

import Tests.Common

--------------------------------------------------------------------------------
                            -- Examples --
--------------------------------------------------------------------------------
testReader :: Int -> Int -> Int
testReader n x = run . flip runReader n $ ask `add` pure x

{-
t1rr' = run t1
    No instance for (Member (Reader Int) Void)
      arising from a use of `t1'
-}

testMultiReader :: Float -> Int -> Float
testMultiReader f n = run . flip runReader f . flip runReader n $ t2
  where t2 = do
          v1 <- ask
          v2 <- ask
          return $ fromIntegral (v1 + (1::Int)) + (v2 + (2::Float))

-- The opposite order of layers
{- If we mess up, we get an error
t2rrr1' = run $ runReader (runReader t2 (20::Float)) (10::Float)
    No instance for (Member (Reader Int) [])
      arising from a use of `t2'
-}

testLocal :: Int -> Int -> Int
testLocal env inc = run $ runReader t3 env
  where t3 = t1 `add` local (+ inc) t1
        t1 = ask `add` return (1 :: Int)
