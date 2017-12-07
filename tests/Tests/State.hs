module Tests.State (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Control.Monad.Freer (run)
import Control.Monad.Freer.State (evalState, execState, get, put, runState)

tests :: TestTree
tests = testGroup "State tests"
  [ testProperty "get after put n yields (n, n)"
      $ \n -> testPutGet n 0 == (n, n)
  , testProperty "Final put determines stored state"
      $ \p1 p2 start -> testPutGetPutGetPlus p1 p2 start == (p1 + p2, p2)
  , testProperty "If only getting, start state determines outcome"
      $ \start -> testGetStart start == (start, start)
  , testProperty "testEvalState: evalState discards final state"
      $ \n -> testEvalState n == n
  , testProperty "testExecState: execState returns final state"
      $ \n -> testExecState n == n
  ]

testPutGet :: Int -> Int -> (Int, Int)
testPutGet n start = run $ runState start go
  where
    go = put n >> get

testPutGetPutGetPlus :: Int -> Int -> Int -> (Int, Int)
testPutGetPutGetPlus p1 p2 start = run $ runState start go
  where
    go = do
      put p1
      x <- get
      put p2
      y <- get
      pure (x + y)

testGetStart :: Int -> (Int, Int)
testGetStart = run . flip runState get

testEvalState :: Int -> Int
testEvalState = run . flip evalState go
  where
    go = do
      x <- get
      -- Destroy the previous state.
      put (0 :: Int)
      pure x

testExecState :: Int -> Int
testExecState n = run $ execState 0 (put n)
