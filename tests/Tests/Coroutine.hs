-- This is necessary to work around a weird infinite loop bug in GHC 8.0.x. I
-- have no idea what causes it, but disabling these extensions in this module
-- avoids the problem.
{-# LANGUAGE NoGADTs #-}
{-# LANGUAGE NoMonoLocalBinds #-}

module Tests.Coroutine (tests) where

import Control.Monad (unless)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Control.Monad.Freer (Eff, Members, run)
import Control.Monad.Freer.Coroutine
  ( Status(Continue, Done)
  , Yield
  , runC
  , yield
  )
import Control.Monad.Freer.State (State, modify, runState)

tests :: TestTree
tests = testGroup "Coroutine Eff tests"
    [ testProperty "Counting consecutive pairs of odds"
        $ \list -> runTestCoroutine list == countOddDuoPrefix list
    ]

-- | Counts number of consecutive pairs of odd elements at beginning of a list.
countOddDuoPrefix :: [Int] -> Int
countOddDuoPrefix list = count list 0
  where
    count (i1:i2:is) n = if even i1 && even i2 then n else count is (n + 1)
    count _          n = n

runTestCoroutine :: [Int] -> Int
runTestCoroutine list = snd . run $ runState 0 effTestCoroutine
  where
    testCoroutine :: Members '[Yield () Int, State Int] r => Eff r ()
    testCoroutine = do
      -- Yield for two elements and hope they're both odd.
      b <- (&&)
          <$> yield () (even :: Int -> Bool)
          <*> yield () (even :: Int -> Bool)
      unless b $ modify (+ (1 :: Int)) >> testCoroutine

    effTestCoroutine = runC testCoroutine >>= handleStatus list
      where
        handleStatus _      (Done ())       = pure ()
        handleStatus (i:is) (Continue () k) = k i >>= handleStatus is
        handleStatus []     _               = pure ()
