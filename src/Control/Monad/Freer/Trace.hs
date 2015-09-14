{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

{-|
Module      : Control.Monad.Freer.Trace
Description : Composable Trace effects
Copyright   : Alej Cabrera 2015
License     : BSD-3
Maintainer  : cpp.cabrera@gmail.com
Stability   : experimental
Portability : POSIX

Composable handler for Trace effects. Trace allows one to debug the
operation of sequences of effects by outputing to the console.

Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a
starting point.

-}
module Control.Monad.Freer.Trace (
  Trace,
  trace,
  runTrace
) where

import Control.Monad.Freer.Internal

-- | A Trace effect; takes a String and performs output
data Trace v where
  Trace :: String -> Trace ()

-- | Printing a string in a trace
trace :: Member Trace r => String -> Eff r ()
trace = send . Trace

-- | An IO handler for Trace effects
runTrace :: Eff '[Trace] w -> IO w
runTrace (Val x) = return x
runTrace (E u q) = case decomp u of
     Right (Trace s) -> putStrLn s >> runTrace (qApp q ())
     Left _          -> error "runTrace:Left - This should never happen"
