module Capitalize
  ( Capitalize
  , capitalize
  , runCapitalize
  ) where

import Data.Char (toUpper)

import Control.Monad.Freer (Eff, Member, HasLen, interpret, send)

data Capitalize v where
  Capitalize :: String -> Capitalize String

capitalize :: Member Capitalize r => String -> Eff r String
capitalize = send . Capitalize

runCapitalize :: HasLen r => Eff (Capitalize ': r) w -> Eff r w
runCapitalize = interpret $ \(Capitalize s) -> pure (map toUpper s)
