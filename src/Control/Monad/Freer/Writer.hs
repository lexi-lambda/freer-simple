{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
-- |
-- Module:       Control.Monad.Freer.Writer
-- Description:  Composable Writer effects.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  POSIX
--
-- 'Writer' effects, for writing changes to an attached environment.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Writer (
  Writer(..),
  tell,
  runWriter
) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

import Control.Monad.Freer.Internal

-- | Writer effects - send outputs to an effect environment
data Writer o x where
  Writer :: o -> Writer o ()

-- | Send a change to the attached environment
tell :: Member (Writer o) r => o -> Eff r ()
tell o = send $ Writer o

-- | Simple handler for Writer effects
runWriter :: Monoid o => Eff (Writer o ': r) a -> Eff r (a,o)
runWriter = handleRelay (\x -> return (x,mempty))
                  (\ (Writer o) k -> k () >>= \ (x,l) -> return (x,o `mappend` l))
