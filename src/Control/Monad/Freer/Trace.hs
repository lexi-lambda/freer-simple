{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       Control.Monad.Freer.Trace
-- Description:  Composable Trace effects.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Composable handler for 'Trace' effects. Trace allows one to debug the
-- operation of sequences of effects by outputing to the console.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Trace
    ( Trace(..)
    , trace
    , runTrace
    )
  where

import Control.Monad ((>>), return)
import Data.Function ((.))
import Data.String (String)
import System.IO (IO, putStrLn)

import Control.Monad.Freer.Internal (Eff(E, Val), Member, extract, qApp, send)


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
