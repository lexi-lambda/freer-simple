{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Freer.Exception (
  Exc,
  throwError,
  runError,
  catchError
) where

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
