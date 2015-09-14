{-|
Module      : Control.Monad.Freer
Description : Freer - an extensible effects library
Copyright   : Alej Cabrera 2015
License     : BSD-3
Maintainer  : cpp.cabrera@gmail.com
Stability   : experimental
Portability : POSIX

-}
module Control.Monad.Freer (
  Member,
  Eff,
  run,
  send,

  NonDetEff(..),
  makeChoiceA,
  msplit
) where

import Control.Monad.Freer.Internal
