{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Control.Monad.Freer.Exception (
  Exc,
  throwError,
  runError,
  catchError
) where

import Control.Monad
import Control.Monad.Freer.Reader -- for examples
import Control.Monad.Freer.State  -- for examples
import Control.Monad.Freer.Internal

--------------------------------------------------------------------------------
                           -- Exceptions --
--------------------------------------------------------------------------------
-- | Exceptions of the type e; no resumption
newtype Exc e v = Exc e

-- The type is inferred
throwError :: (Member (Exc e) r) => e -> Eff r a
throwError e = send (Exc e)

runError :: Eff (Exc e ': r) a -> Eff r (Either e a)
runError =
   handleRelay (return . Right) (\ (Exc e) _k -> return (Left e))

-- The handler is allowed to rethrow the exception
catchError :: Member (Exc e) r =>
        Eff r a -> (e -> Eff r a) -> Eff r a
catchError m handle = interpose return (\(Exc e) _k -> handle e) m


--------------------------------------------------------------------------------
                       -- Tests and Examples --
--------------------------------------------------------------------------------
add = liftM2 (+)

-- The type is inferred
et1 :: Eff r Int
et1 = return 1 `add` return 2

et1r :: Bool
et1r = 3 == run et1

-- The type is inferred
-- et2 :: Member (Exc Int) r => Eff r Int
et2 = return 1 `add` throwError (2::Int)

-- The following won't type: unhandled exception!
-- ex2rw = run et2
{-
    No instance for (Member (Exc Int) Void)
      arising from a use of `et2'
-}

-- The inferred type shows that ex21 is now pure
et21 :: Eff r (Either Int Int)
et21 = runError et2

et21r = Left 2 == run et21


-- The example from the paper
newtype TooBig = TooBig Int deriving (Eq, Show)
-- The type is inferred
ex2 :: Member (Exc TooBig) r => Eff r Int -> Eff r Int
ex2 m = do
  v <- m
  if v > 5 then throwError (TooBig v)
     else return v

-- specialization to tell the type of the exception
runErrBig :: Eff (Exc TooBig ': r) a -> Eff r (Either TooBig a)
runErrBig = runError


-- exceptions and state
incr :: Member (State Int) r => Eff r ()
incr = get >>= put . (+ (1::Int))

tes1 :: (Member (State Int) r, Member (Exc [Char]) r) => Eff r b
tes1 = do
 incr
 throwError "exc"

ter1 :: Bool
ter1 = ((Left "exc" :: Either String Int,2) ==) $
       run $ runState (runError tes1) (1::Int)


ter2 :: Bool
ter2 = ((Left "exc" :: Either String (Int,Int)) ==) $
       run $ runError (runState tes1 (1::Int))


teCatch :: Member (Exc String) r => Eff r a -> Eff r [Char]
teCatch m = catchError (m >> return "done") (\e -> return (e::String))

ter3 :: Bool
ter3 = ((Right "exc" :: Either String String,2) ==) $
       run $ runState (runError (teCatch tes1)) (1::Int)

ter4 :: Bool
ter4 = ((Right ("exc",2) :: Either String (String,Int)) ==) $
       run $ runError (runState (teCatch tes1) (1::Int))

ex2r = runReader (runErrBig (ex2 ask)) (5::Int)

ex2rr = Right 5 == run ex2r

ex2rr1 = (Left (TooBig 7) ==) $
         run $ runReader (runErrBig (ex2 ask)) (7::Int)

-- Different order of handlers (layers)
ex2rr2 = (Left (TooBig 7) ==) $
         run $ runErrBig (runReader (ex2 ask) (7::Int))
