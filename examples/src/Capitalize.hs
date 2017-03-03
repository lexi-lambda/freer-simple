{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
module Capitalize where

import Data.Char (toUpper)

import Control.Monad.Freer
import Control.Monad.Freer.Internal

data Capitalize v where
    Capitalize :: String -> Capitalize String

capitalize :: Member Capitalize r => String -> Eff r String
capitalize = send . Capitalize

runCapitalizeM :: Eff (Capitalize ': r) w -> Eff r w
runCapitalizeM (Val x) = return x
runCapitalizeM (E u q) = case decomp u of
    Right (Capitalize s) -> runCapitalizeM (qApp q (map toUpper s))
    Left u'              -> E u' (tsingleton (runCapitalizeM . qApp q))
