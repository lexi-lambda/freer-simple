{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Control.Monad.Freer.Trace (
  Trace,
  trace,
  runTrace
) where

import Control.Monad
import Control.Monad.Freer.Internal
import Control.Monad.Freer.Reader -- for examples

--------------------------------------------------------------------------------
                    -- Tracing (debug printing) --
--------------------------------------------------------------------------------
data Trace v where
  Trace :: String -> Trace ()

-- Printing a string in a trace
trace :: Member Trace r => String -> Eff r ()
trace = send . Trace

-- The handler for IO request: a terminal handler
runTrace :: Eff '[Trace] w -> IO w
runTrace (Val x) = return x
runTrace (E u q) = case decomp u of
     Right (Trace s) -> putStrLn s >> runTrace (qApp q ())
     -- Nothing more can occur

--------------------------------------------------------------------------------
                       -- Tests and Examples --
--------------------------------------------------------------------------------
-- Higher-order effectful function
-- The inferred type shows that the Trace affect is added to the effects
-- of r
mapMdebug:: (Show a, Member Trace r) =>
     (a -> Eff r b) -> [a] -> Eff r [b]
mapMdebug f [] = return []
mapMdebug f (h:t) = do
 trace $ "mapMdebug: " ++ show h
 h' <- f h
 t' <- mapMdebug f t
 return (h':t')

add = liftM2 (+)

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
