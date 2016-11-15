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
  send,

  NonDetEff(..),
  makeChoiceA,
  msplit
) where

import Control.Monad.Freer.Internal
