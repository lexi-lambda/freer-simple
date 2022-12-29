-- |
-- Module:       Control.Monad.Freer.Trace
-- Description:  Composable Trace effects.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King
-- License:      BSD3
-- Maintainer:   Alexis King <lexi.lambda@gmail.com>
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Composable handler for 'Trace' effects. Trace allows one to debug the
-- operation of sequences of effects by outputting to the console.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Trace
  ( Trace(..)
  , trace
  , runTrace
  ) where

import Control.Monad.Freer.Internal (Eff(..), Member, extract, qApp, send)

-- | A Trace effect; takes a 'String' and performs output.
data Trace a where
  Trace :: String -> Trace ()

-- | Printing a string in a trace.
trace :: Member Trace effs => String -> Eff effs ()
trace = send . Trace

-- | An 'IO' handler for 'Trace' effects.
runTrace :: Eff '[Trace] a -> IO a
runTrace (Val x) = return x
runTrace (E u q) = case extract u of
  Trace s -> putStrLn s >> runTrace (qApp q ())
