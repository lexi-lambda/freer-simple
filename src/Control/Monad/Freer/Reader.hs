{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Control.Monad.Freer.Reader
Description : Reader effects, for encapsulating an environment
Copyright   : Alej Cabrera 2015
License     : BSD-3
Maintainer  : cpp.cabrera@gmail.com
Stability   : experimental
Portability : POSIX

Composable handler for Reader effects. Handy for encapsulating an
environment with immutable state for interpreters.

Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a
starting point.

-}
module Control.Monad.Freer.Reader (
  Reader(..),

  ask,
  runReader,
  local
) where

import Control.Monad.Freer.Internal

-- |
data Reader e v where
  Reader :: Reader e e

-- | Request a value for the environment
ask :: (Member (Reader e) r) => Eff r e
ask = send Reader

-- | Handler for reader effects
runReader :: Eff (Reader e ': r) w -> e -> Eff r w
runReader m e = handleRelay return (\Reader k -> k e) m

-- |
-- Locally rebind the value in the dynamic environment
-- This function is like a relay; it is both an admin for Reader requests,
-- and a requestor of them
local :: forall e a r. Member (Reader e) r =>
         (e -> e) -> Eff r a -> Eff r a
local f m = do
  e0 <- ask
  let e = f e0
  let h :: Reader e v -> Arr r v a -> Eff r a
      h Reader g = g e
  interpose return h m
