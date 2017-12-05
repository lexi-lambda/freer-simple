{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       Control.Monad.Freer.Error
-- Description:  An Error effect and handler.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Composable handler for Error effects. Communicates success\/failure via an
-- 'Either' type.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Error
    ( Error(..)
    , throwError
    , runError
    , catchError
    , handleError
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
newtype Error e a = Error e

-- | Throws an error carrying information of type @e :: *@.
throwError :: Member (Error e) effs => e -> Eff effs a
throwError e = send (Error e)

-- | Handler for exception effects. If there are no exceptions thrown, returns
-- 'Right'. If exceptions are thrown and not handled, returns 'Left', while
-- interrupting the execution of any other effect handlers.
runError :: Eff (Error e ': effs) a -> Eff effs (Either e a)
runError = handleRelay (pure . Right) (\(Error e) _k -> pure (Left e))

-- | A catcher for Exceptions. Handlers are allowed to rethrow exceptions.
catchError
  :: Member (Error e) effs
  => Eff effs a
  -> (e -> Eff effs a)
  -> Eff effs a
catchError m handle = interpose pure (\(Error e) _ -> handle e) m

-- | A catcher for Exceptions. Handlers are /not/ allowed to rethrow exceptions.
handleError
  :: Eff (Error e ': effs) a
  -> (e -> Eff effs a)
  -> Eff effs a
handleError m handle = handleRelay pure (\(Error e) _ -> handle e) m
