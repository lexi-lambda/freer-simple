{-|
Module      : Control.Monad.Freer
Description : Freer - an extensible effects library
Copyright   : Allele Dev 2016
License     : BSD-3
Maintainer  : allele.dev@gmail.com
Stability   : experimental
Portability : POSIX

-}
module Control.Monad.Freer (
  Member,
  Members,
  Eff,
  run,
  runM,
  handleRelay,
  handleRelayS,
  send,
  Arr,

  NonDetEff(..),
  makeChoiceA,
  msplit
) where

import Control.Monad.Freer.Internal
