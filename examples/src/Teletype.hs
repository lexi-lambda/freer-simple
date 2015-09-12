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
runTeletype (E u q) = case decomp u of
              Right (PutStrLn msg) -> putStrLn msg  >> runTeletype (qApp q ())
              Right GetLine        -> getLine      >>= \s -> runTeletype (qApp q s)
              Right ExitSuccess    -> exitSuccess
              Left  _              -> error "This cannot happen"

--------------------------------------------------------------------------------
                        -- Pure Interpreter --
--------------------------------------------------------------------------------
runTeletypePure :: [String] -> Eff '[Teletype] w -> [String]
runTeletypePure inputs req = reverse (go inputs req [])
  where go :: [String] -> Eff '[Teletype] w -> [String] -> [String]
        go _      (Val _) acc = acc
        go []     _       acc = acc
        go (x:xs) (E u q) acc = case decomp u of
          Right (PutStrLn msg) -> go (x:xs) (qApp q ()) (msg:acc)
          Right GetLine        -> go xs     (qApp q x) acc
          Right ExitSuccess    -> go xs     (Val ())   acc
          Left _               -> go xs     (Val ())   acc
