{-# LANGUAGE TemplateHaskell #-}
module Tests.TH where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Control.Monad.Freer (Eff, run, interpret, type(~>))
import Control.Monad.Freer.TH (makeEffect)

-- Create a test GADT for our effects.
data Prepender next where
  PrependSomething :: String -> Prepender String

-- Make TH generate our effect functions.
makeEffect ''Prepender

tests :: TestTree
tests = testGroup
  "TH tests"
  [ testProperty "Prepender uses generated effects"
      $ \s -> testGeneratedFunction s == ("prepended: " ++ s)
  ]

--------------------------------------------------------------------------------
                            -- Examples --
--------------------------------------------------------------------------------
runPrepender :: Eff (Prepender ': effs) ~> Eff effs
runPrepender = interpret
  (\case
  PrependSomething s -> pure $ "prepended: " ++ s
  )

testGeneratedFunction :: String -> String
testGeneratedFunction s = run . runPrepender $ prependSomething s
