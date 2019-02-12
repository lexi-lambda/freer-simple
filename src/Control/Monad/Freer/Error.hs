-- |
-- Module:       Control.Monad.Freer.Error
-- Description:  An Error effect and handler.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King
-- License:      BSD3
-- Maintainer:   Alexis King <lexi.lambda@gmail.com>
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
  ) where

import Control.Monad.Freer (Eff, Member, interposeWith, interpretWith, send)
import Control.Monad.Freer.Internal (handleRelay)

-- | Exceptions of the type @e :: *@ with no resumption.
newtype Error e r where
  Error :: e -> Error e r

-- | Throws an error carrying information of type @e :: *@.
throwError :: forall e effs a. Member (Error e) effs => e -> Eff effs a
throwError e = send (Error e)
{-# INLINE throwError #-}

-- | Handler for exception effects. If there are no exceptions thrown, returns
-- 'Right'. If exceptions are thrown and not handled, returns 'Left', while
-- interrupting the execution of any other effect handlers.
runError :: forall e effs a. Eff (Error e ': effs) a -> Eff effs (Either e a)
runError = handleRelay (pure . Right) (\(Error e) _ -> pure (Left e))
{-# INLINE runError #-}

-- | A catcher for Exceptions. Handlers are allowed to rethrow exceptions.
catchError
  :: forall e effs a
   . Member (Error e) effs
  => Eff effs a
  -> (e -> Eff effs a)
  -> Eff effs a
catchError m handle = interposeWith (\(Error e) _ -> handle e) m
{-# INLINE catchError #-}

-- | A catcher for Exceptions. Handlers are /not/ allowed to rethrow exceptions.
handleError
  :: forall e effs a
   . Eff (Error e ': effs) a
  -> (e -> Eff effs a)
  -> Eff effs a
handleError m handle = interpretWith (\(Error e) _ -> handle e) m
{-# INLINE handleError #-}
