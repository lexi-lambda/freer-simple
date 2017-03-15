{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Tests.Coroutine (tests) where

import Prelude ((+), even)

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad ((>>=), (>>), unless)
import Data.Bool (Bool, (&&))
import Data.Eq ((==))
import Data.Function (($))
import Data.Int (Int)
import Data.Tuple (snd)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Control.Monad.Freer (Eff, Members, run)
import Control.Monad.Freer.Coroutine (Yield, Status(Continue, Done), runC, yield)
import Control.Monad.Freer.State (State, runState, modify)


tests :: TestTree
tests = testGroup "Coroutine Eff tests"
  [ testProperty "Counting consecutive pairs of odds"
      (\list -> runTestCoroutine list == countOddDuoPrefix list)
  ]

-- | Counts number of consecutive pairs of odd elements at beginning of a list.
countOddDuoPrefix :: [Int] -> Int
countOddDuoPrefix list = count list 0
  where
    count (i1:i2:is) n = if even i1 && even i2 then n else count is (n+1)
    count _ n = n

runTestCoroutine :: [Int] -> Int
runTestCoroutine list = snd $ run $ runState effTestCoroutine 0
  where
    testCoroutine :: (Members '[Yield () Int, State Int] r) => Eff r ()
    testCoroutine = do
      -- yield for two elements and hope they're both odd
      b <- (&&)
        <$> yield () (even :: Int -> Bool)
        <*> yield () (even :: Int -> Bool)
      unless b (modify ((+1) :: Int -> Int) >> testCoroutine)

    effTestCoroutine = do
      status <- runC testCoroutine
      handleStatus list status
        where
          handleStatus _ Done = pure ()
          handleStatus (i:is) (Continue () k) = k i >>= handleStatus is
          handleStatus [] _ = pure ()
