{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
module Tests.Exception (tests)
  where

import Prelude ((+))

import Control.Applicative ((<*>), pure)
import Control.Monad ((>>), (>>=))
import Data.Either (Either(Left, Right))
import Data.Eq (Eq((==)))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Ord ((>))
import Data.String (String)
import Text.Show (Show)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import Control.Monad.Freer (Eff, Member, Members, run)
import Control.Monad.Freer.Error (Error, catchError, runError, throwError)
import Control.Monad.Freer.Reader (ask, runReader)
import Control.Monad.Freer.State (State, get, put, runState)


tests :: TestTree
tests = testGroup "Exception Eff tests"
    [ testProperty "Error takes precedence"
        $ \x y -> testExceptionTakesPriority x y == Left y
    , testCase "uncaught: runState (runError t)"
        $ ter1 @?= (Left "exc", 2)
    , testCase "uncaught: runError (runState t)"
        $ ter2 @?= Left "exc"
    , testCase "caught: runState (runError t)"
        $ ter3 @?= (Right "exc", 2)
    , testCase "caught: runError (runState t)"
        $ ter4 @?= Right ("exc", 2)
    , testCase "success: runReader (runErrBig t)"
        $ ex2rr @?= Right 5
    , testCase "uncaught: runReader (runErrBig t)"
        $ ex2rr1 @?= Left (TooBig 7)
    , testCase "uncaught: runErrBig (runReader t)"
        $ ex2rr2 @?= Left (TooBig 7)
    ]

testExceptionTakesPriority :: Int -> Int -> Either Int Int
testExceptionTakesPriority x y = run $ runError (go x y)
  where
    go a b = (+) <$> pure a <*> throwError b

-- The following won't type: unhandled exception!
-- ex2rw = run et2
{-
    No instance for (Member (Error Int) Void)
      arising from a use of `et2'
-}

-- Exceptions and state.
incr :: Member (State Int) r => Eff r ()
incr = get >>= put . (+ (1 :: Int))

tes1 :: (Members '[State Int, Error String] r) => Eff r b
tes1 = incr >> throwError "exc"

ter1 :: (Either String Int, Int)
ter1 = run $ runState (runError tes1) (1 :: Int)

ter2 :: Either String (String, Int)
ter2 = run $ runError (runState tes1 (1 :: Int))

teCatch :: Member (Error String) r => Eff r a -> Eff r String
teCatch m = (m >> pure "done") `catchError` \e -> pure (e :: String)

ter3 :: (Either String String, Int)
ter3 = run $ runState (runError (teCatch tes1)) (1 :: Int)

ter4 :: Either String (String, Int)
ter4 = run $ runError (runState (teCatch tes1) (1 :: Int))

-- | The example from the paper.
newtype TooBig = TooBig Int
  deriving (Eq, Show)

ex2 :: Member (Error TooBig) r => Eff r Int -> Eff r Int
ex2 m = do
    v <- m
    if v > 5
        then throwError (TooBig v)
        else pure v

-- | Specialization to tell the type of the exception.
runErrBig :: Eff (Error TooBig ': r) a -> Eff r (Either TooBig a)
runErrBig = runError

ex2rr :: Either TooBig Int
ex2rr = run $ runReader (runErrBig (ex2 ask)) (5 :: Int)

ex2rr1 :: Either TooBig Int
ex2rr1 = run $ runReader (runErrBig (ex2 ask)) (7 :: Int)

-- | Different order of handlers (layers).
ex2rr2 :: Either TooBig Int
ex2rr2 = run $ runErrBig (runReader (ex2 ask) (7 :: Int))
