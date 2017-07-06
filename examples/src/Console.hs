{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Console
  ( Console
  , exitSuccess'
  , getLine'
  , putStrLn'
  , runConsole
  , runConsoleM
  , runConsolePure
  , runConsolePureM
  ) where

import Prelude (error)

import Control.Applicative (pure)
import Control.Monad ((>>=), (>>))
import Data.Either (either)
import Data.Function (($), (&), (.), const, flip)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (String)
import Data.Tuple (fst, snd)
import System.Exit (exitSuccess)
import System.IO (IO, getLine, putStrLn)

import Control.Monad.Freer (Eff, Member, interpret, reinterpret3, send, run, runM)
import Control.Monad.Freer.Exception (Exc, runError, throwError)
import Control.Monad.Freer.State (State, get, put, runState)
import Control.Monad.Freer.Writer (Writer, runWriter, tell)


-------------------------------------------------------------------------------
                          -- Effect Model --
-------------------------------------------------------------------------------
data Console s where
  PutStrLn    :: String -> Console ()
  GetLine     :: Console String
  ExitSuccess :: Console ()

putStrLn' :: Member Console r => String -> Eff r ()
putStrLn' = send . PutStrLn

getLine'  :: Member Console r => Eff r String
getLine' = send GetLine

exitSuccess' :: Member Console r => Eff r ()
exitSuccess' = send ExitSuccess

-------------------------------------------------------------------------------
                     -- Effectful Interpreter Simple --
-------------------------------------------------------------------------------
runConsole :: Eff '[Console, IO] a -> IO a
runConsole req = runM (interpret go req)
  where
    go :: Console a -> Eff '[IO] a
    go (PutStrLn msg) = send (putStrLn msg)
    go GetLine = send getLine
    go ExitSuccess = send exitSuccess

-------------------------------------------------------------------------------
                        -- Pure Interpreter Simple --
-------------------------------------------------------------------------------
runConsolePure :: [String] -> Eff '[Console] w -> [String]
runConsolePure inputs req = snd . fst $
    run (runWriter (runState (runError (reinterpret3 go req)) inputs))
  where
    go :: Console v -> Eff '[Exc (), State [String], Writer [String]] v
    go (PutStrLn msg) = tell [msg]
    go GetLine = get >>= \case
      [] -> error "not enough lines"
      (x:xs) -> put xs >> pure x
    go ExitSuccess = throwError ()

-------------------------------------------------------------------------------
                     -- Effectful Interpreter for Deeper Stack --
-------------------------------------------------------------------------------
runConsoleM :: forall effs a. Member IO effs
            => Eff (Console ': effs) a -> Eff effs a
runConsoleM = interpret go
  where
    go :: forall b. Console b -> Eff effs b
    go (PutStrLn msg) = send (putStrLn msg)
    go GetLine = send getLine
    go ExitSuccess = send exitSuccess

-------------------------------------------------------------------------------
                     -- Pure Interpreter for Deeper Stack --
-------------------------------------------------------------------------------
runConsolePureM
  :: forall effs w
   . [String]
  -> Eff (Console ': effs) w
  -> Eff effs (Maybe w, [String], [String])
runConsolePureM inputs req = do
    ((x, inputs'), output) <- reinterpret3 go req
      & runError & flip runState inputs & runWriter
    pure (either (const Nothing) Just x, inputs', output)
  where
    go :: Console v
       -> Eff (Exc () ': State [String] ': Writer [String] ': effs) v
    go (PutStrLn msg) = tell [msg]
    go GetLine = get >>= \case
      [] -> error "not enough lines"
      (x:xs) -> put xs >> pure x
    go ExitSuccess = throwError ()
