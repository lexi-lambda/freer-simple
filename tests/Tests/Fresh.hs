{-# LANGUAGE NoImplicitPrelude #-}
module Tests.Fresh (tests)
  where

import Prelude ((-))

import Control.Monad (replicateM)
import Data.Eq ((==))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (last)
import Data.Ord ((>))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.QuickCheck ((==>), testProperty)

import Control.Monad.Freer (Eff, run)
import Control.Monad.Freer.Fresh (fresh, runFresh')


tests :: TestTree
tests = testGroup "Fresh tests"
    [ testCase "Start at 0, refresh twice, yields 1"
        $ testFresh 10 @?= 9
    , testProperty "Freshening n times yields (n-1)"
        $ \n -> n > 0 ==> testFresh n == (n-1)
    ]

makeFresh :: Int -> Eff r Int
makeFresh n = runFresh' (last <$> replicateM n fresh) 0

testFresh :: Int -> Int
testFresh = run . makeFresh
