{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Tests.Exception (
  TooBig(..),

  testExceptionTakesPriority,

  ter1,
  ter2,
  ter3,
  ter4,

  ex2rr,
  ex2rr1,
  ex2rr2,
) where

import Control.Monad.Freer
import Control.Monad.Freer.Exception
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State

import Tests.Common

--------------------------------------------------------------------------------
                       -- Tests and Examples --
--------------------------------------------------------------------------------
testExceptionTakesPriority :: Int -> Int -> Either Int Int
testExceptionTakesPriority x y = run $ runError (go x y)
  where go a b = return a `add` throwError b

-- The following won't type: unhandled exception!
-- ex2rw = run et2
{-
    No instance for (Member (Exc Int) Void)
      arising from a use of `et2'
-}

-- exceptions and state
incr :: Member (State Int) r => Eff r ()
incr = get >>= put . (+ (1::Int))

tes1 :: (Member (State Int) r, Member (Exc String) r) => Eff r b
tes1 = do
 incr
 throwError "exc"

ter1 :: (Either String Int, Int)
ter1 = run $ runState (runError tes1) (1::Int)

ter2 :: Either String (String, Int)
ter2 = run $ runError (runState tes1 (1::Int))

teCatch :: Member (Exc String) r => Eff r a -> Eff r String
teCatch m = catchError (m >> return "done") (\e -> return (e::String))

ter3 :: (Either String String, Int)
ter3 = run $ runState (runError (teCatch tes1)) (1::Int)

ter4 :: Either String (String, Int)
ter4 = run $ runError (runState (teCatch tes1) (1::Int))

-- The example from the paper
newtype TooBig = TooBig Int deriving (Eq, Show)

excTooBig :: Member (Exc TooBig) r => Int -> Eff r Int -> Eff r Int
excTooBig limit m = do
  v <- m
  if v > limit then throwError (TooBig v)
    else return v

ex2 :: Member (Exc TooBig) r => Eff r Int -> Eff r Int
ex2 m = do
  v <- m
  if v > 5 then throwError (TooBig v)
     else return v

-- specialization to tell the type of the exception
runErrBig :: Eff (Exc TooBig ': r) a -> Eff r (Either TooBig a)
runErrBig = runError

ex2rr :: Either TooBig Int
ex2rr = run go
  where go = runReader (runErrBig (ex2 ask)) (5::Int)

ex2rr1 :: Either TooBig Int
ex2rr1 = run $ runReader (runErrBig (ex2 ask)) (7::Int)

-- Different order of handlers (layers)
ex2rr2 :: Either TooBig Int
ex2rr2 = run $ runErrBig (runReader (ex2 ask) (7::Int))
