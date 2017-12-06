module Capitalize
  ( Capitalize
  , capitalize
  , runCapitalizeM
  , runCapitalizeM'
  ) where

import Data.Char (toUpper)

import Control.Monad.Freer (Member, interpret, send)
import Control.Monad.Freer.Internal (Eff(Val, E), decomp, qApp, tsingleton)


data Capitalize v where
  Capitalize :: String -> Capitalize String

capitalize :: Member Capitalize r => String -> Eff r String
capitalize = send . Capitalize

runCapitalizeM :: Eff (Capitalize ': r) w -> Eff r w
runCapitalizeM (Val x) = pure x
runCapitalizeM (E u q) = case decomp u of
  Right (Capitalize s) -> runCapitalizeM (qApp q (map toUpper s))
  Left u'              -> E u' (tsingleton (runCapitalizeM . qApp q))

runCapitalizeM' :: Eff (Capitalize ': r) w -> Eff r w
runCapitalizeM' = interpret $ \(Capitalize s) -> pure (map toUpper s)
