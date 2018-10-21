module Main (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.QuickCheck (testProperty)

import Control.Monad.Freer (run)

import qualified Tests.Coroutine (tests)
import qualified Tests.Exception (tests)
import qualified Tests.Fresh (tests)
import qualified Tests.NonDet (tests)
import qualified Tests.Reader (tests)
import qualified Tests.State (tests)
import qualified Tests.Loop (tests)
import qualified Tests.TH (tests)

--------------------------------------------------------------------------------
                           -- Pure Tests --
--------------------------------------------------------------------------------
addInEff :: Int -> Int -> Int
addInEff x y = run $ (+) <$> pure x <*> pure y

pureTests :: TestTree
pureTests = testGroup "Pure Eff tests"
  [ testProperty "Pure run just works: (+)"
      $ \x y -> addInEff x y == x + y
  ]

--------------------------------------------------------------------------------
                             -- Runner --
--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ pureTests
  , Tests.Coroutine.tests
  , Tests.Exception.tests
  , Tests.Fresh.tests
  , Tests.NonDet.tests
  , Tests.Reader.tests
  , Tests.State.tests
  , Tests.Loop.tests
  , Tests.TH.tests
  ]
