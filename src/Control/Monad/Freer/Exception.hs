{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       Control.Monad.Freer.Exception
-- Description:  An Exception effect and handler.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Composable handler for Exception effects. Communicates success\/failure
-- via an 'Either' type.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Exception
    ( Exc(..)
    , throwError
    , runError
    , catchError
    )
  where

import Control.Applicative (pure)
import Data.Either (Either(Left, Right))
import Data.Function ((.))

import Control.Monad.Freer.Internal (Eff, Member, handleRelay, interpose, send)


--------------------------------------------------------------------------------
                           -- Exceptions --
--------------------------------------------------------------------------------

-- | Exceptions of the type @e :: *@ with no resumption.
newtype Exc e a = Exc e

-- | Throws an error carrying information of type @e :: *@.
throwError :: Member (Exc e) effs => e -> Eff effs a
throwError e = send (Exc e)

-- | Handler for exception effects. If there are no exceptions thrown, returns
-- 'Right'. If exceptions are thrown and not handled, returns 'Left', while
-- interrupting the execution of any other effect handlers.
runError :: Eff (Exc e ': effs) a -> Eff effs (Either e a)
runError = handleRelay (pure . Right) (\(Exc e) _k -> pure (Left e))

-- | A catcher for Exceptions. Handlers are allowed to rethrow exceptions.
catchError
    :: Member (Exc e) effs
    => Eff effs a
    -> (e -> Eff effs a)
    -> Eff effs a
catchError m handle = interpose pure (\(Exc e) _k -> handle e) m
