{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Freer
import Teletype

runner :: (Member Teletype r) => Eff r ()
runner = do
  x <- getLine'
  _ <- getLine'
  putStrLn' x
  z <- getLine'
  putStrLn' z
  putStrLn' x
  putStrLn' x

main :: IO ()
main = do
  let xs = runTeletypePure ["cat", "fish", "dog", "bird"] runner
  print xs
  runTeletype runner
