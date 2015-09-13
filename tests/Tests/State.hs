{-# LANGUAGE FlexibleContexts #-}
module Tests.State (
  testPutGet,
  testPutGetPutGetPlus,
  testGetStart
) where

import Control.Monad.Freer
import Control.Monad.Freer.State

testPutGet :: Int -> Int -> (Int,Int)
testPutGet n start = run (runState go start)
  where go = put n >> get

testPutGetPutGetPlus :: Int -> Int -> Int -> (Int,Int)
testPutGetPutGetPlus p1 p2 start = run (runState go start)
  where go = do
          put p1
          x <- get
          put p2
          y <- get
          return (x+y)

testGetStart :: Int -> (Int,Int)
testGetStart = run . runState get
