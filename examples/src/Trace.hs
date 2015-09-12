{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Trace where

import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Trace

import Common

--------------------------------------------------------------------------------
                       -- Tests and Examples --
--------------------------------------------------------------------------------
-- Higher-order effectful function
-- The inferred type shows that the Trace affect is added to the effects
-- of r
mapMdebug:: (Show a, Member Trace r) =>
     (a -> Eff r b) -> [a] -> Eff r [b]
mapMdebug _ [] = return []
mapMdebug f (h:t) = do
 trace $ "mapMdebug: " ++ show h
 h' <- f h
 t' <- mapMdebug f t
 return (h':t')

tMd :: IO [Int]
tMd = runTrace $ runReader (mapMdebug f [1..5]) (10::Int)
 where f x = ask `add` return x
{-
mapMdebug: 1
mapMdebug: 2
mapMdebug: 3
mapMdebug: 4
mapMdebug: 5
[11,12,13,14,15]
-}

-- duplicate layers
tdup :: IO ()
tdup = runTrace $ runReader m (10::Int)
 where
 m = do
     runReader tr (20::Int)
     tr
 tr = do
      v <- ask
      trace $ "Asked: " ++ show (v::Int)
{-
Asked: 20
Asked: 10
-}
