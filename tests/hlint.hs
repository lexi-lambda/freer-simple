{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       Main
-- Description:  HLint tests executor
-- Copyright:    (c) 2015-2016, Ixperta Solutions s.r.o.
-- License:      BSD3
--
-- Stability:    stable
-- Portability:  portable
--
-- HLint tests executor.
module Main (main)
  where

import Control.Monad (unless)
import Data.Function (($))
import Data.List (null)
import Data.Monoid ((<>))
import System.IO (IO, putStrLn)
import System.Exit (exitFailure)

import Language.Haskell.HLint (hlint)


main :: IO ()
main = do
    putStrLn "" -- less confusing output, test-framework does this too
    hints <- hlint $ hlintOpts <> ["."]
    unless (null hints) exitFailure
  where
    hlintOpts =
        [ "-XNoPatternSynonyms"
        ]
