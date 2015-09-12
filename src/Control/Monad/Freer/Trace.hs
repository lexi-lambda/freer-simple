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

import Control.Monad.Freer.Internal

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
     Left _          -> error "runTrace:Left - This should never happen"
