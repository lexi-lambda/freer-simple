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

-- Create a more complicated test GADT
data Complicated a where
  Mono :: Int -> Complicated Bool
  Poly :: a -> Complicated a
  PolyIn :: a -> Complicated Bool
  PolyOut :: Int -> Complicated a
  Lots :: a -> b -> c -> d -> e -> f -> Complicated ()
  Nested :: Maybe b -> Complicated (Maybe a)
  MultiNested :: (Maybe a, [b]) -> Complicated (Maybe a, [b])
  Existential :: (forall e. e -> Maybe e) -> Complicated a
  LotsNested :: Maybe a -> [b] -> (c, c) -> Complicated (a, b, c)
  Dict :: (Ord a) => a -> Complicated a
  MultiDict :: (Eq a, Ord b, Enum a, Num c) => a -> b -> c -> Complicated ()

-- Make TH generate our effect functions.
makeEffect ''Complicated
