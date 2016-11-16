{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Teletype where

import Control.Monad.Freer
import Control.Monad.Freer.Internal
import System.Exit hiding (ExitSuccess)

--------------------------------------------------------------------------------
                          -- Effect Model --
--------------------------------------------------------------------------------
data Teletype s where
  PutStrLn    :: String -> Teletype ()
  GetLine     :: Teletype String
  ExitSuccess :: Teletype ()

putStrLn' :: Member Teletype r => String -> Eff r ()
putStrLn' = send . PutStrLn

getLine'  :: Member Teletype r => Eff r String
getLine' = send GetLine

exitSuccess' :: Member Teletype r => Eff r ()
exitSuccess' = send ExitSuccess

--------------------------------------------------------------------------------
                     -- Effectful Interpreter --
--------------------------------------------------------------------------------
runTeletype :: Eff '[Teletype] w -> IO w
runTeletype (Val x) = return x
runTeletype (E u q) = case extract u of
              (PutStrLn msg) -> putStrLn msg  >> runTeletype (qApp q ())
              GetLine        -> getLine      >>= \s -> runTeletype (qApp q s)
              ExitSuccess    -> exitSuccess

--------------------------------------------------------------------------------
                        -- Pure Interpreter --
--------------------------------------------------------------------------------
runTeletypePure :: [String] -> Eff '[Teletype] w -> [String]
runTeletypePure inputs req = reverse (go inputs req [])
  where go :: [String] -> Eff '[Teletype] w -> [String] -> [String]
        go _      (Val _) acc = acc
        go []     _       acc = acc
        go (x:xs) (E u q) acc = case extract u of
          (PutStrLn msg) -> go (x:xs) (qApp q ()) (msg:acc)
          GetLine        -> go xs     (qApp q x) acc
          ExitSuccess    -> go xs     (Val ())   acc
