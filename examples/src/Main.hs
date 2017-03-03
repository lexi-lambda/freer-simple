{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad (forever, when)
import System.Environment (getArgs)

import Control.Monad.Freer

import Capitalize
import Teletype

-------------------------------------------------------------------------------
-- Example
-------------------------------------------------------------------------------
capitalizingTeletype :: (Member Teletype r, Member Capitalize r) => Eff r ()
capitalizingTeletype = forever $ do
    putStrLn' "Send something to capitalize..."
    l <- getLine'
    when (null l) exitSuccess'
    capitalize l >>= putStrLn'
-------------------------------------------------------------------------------

mainPure :: IO ()
mainPure = print . run
    . runTeletypePureM ["cat", "fish", "dog", "bird", ""]
    $ runCapitalizeM capitalizingTeletype

mainConsoleA :: IO ()
mainConsoleA = runM (runTeletypeM (runCapitalizeM capitalizingTeletype))
--             |     |             |              |
--      IO () -'     |             |              |
--     Eff '[IO] () -'             |              |
--         Eff '[Teletype, IO] () -'              |
--            Eff '[Capitalize, Teletype, IO] () -'

mainConsoleB :: IO ()
mainConsoleB = runM (runCapitalizeM (runTeletypeM capitalizingTeletype))
--             |     |             |              |
--      IO () -'     |             |              |
--     Eff '[IO] () -'             |              |
--       Eff '[Capitalize, IO] () -'              |
--            Eff '[Teletype, Capitalize, IO] () -'

main :: IO ()
main = getArgs >>= \case
    ["pure"] -> mainPure
    ["consoleA"] -> mainConsoleA
    ["consoleB"] -> mainConsoleB
    _ -> putStrLn "Bad argument. Look into source for possible values."
