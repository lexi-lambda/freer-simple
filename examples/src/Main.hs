{-# LANGUAGE DataKinds #-}
module Main where

import Control.Monad.Freer
import Teletype

runner :: Eff '[Teletype] ()
runner = do
  x <- getLine'
  putStrLn' x
  y <- getLine'
  putStrLn' y

main :: IO ()
main = do
  let xs = runTeletypePure ["cat", "fish"] runner
  print xs
  runTeletype runner
