module Tests.Fresh (tests) where

import Control.Monad (replicateM)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.QuickCheck ((==>), testProperty)

import Control.Monad.Freer (Eff, HasLen, run)
import Control.Monad.Freer.Fresh (fresh, runFresh)

tests :: TestTree
tests = testGroup "Fresh tests"
  [ testCase "Start at 0, refresh twice, yields 1"
      $ testFresh 10 @?= 9
  , testProperty "Freshening n times yields (n-1)"
      $ \n -> n > 0 ==> testFresh n == (n-1)
  ]

makeFresh :: HasLen r => Int -> Eff r Int
makeFresh n = fst <$> runFresh 0 (last <$> replicateM n fresh)

testFresh :: Int -> Int
testFresh = run . makeFresh
