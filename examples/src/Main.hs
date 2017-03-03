{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad (forever, when)
import System.Environment (getArgs)

import Control.Monad.Freer

import Capitalize
import Console

-------------------------------------------------------------------------------
-- Example
-------------------------------------------------------------------------------
capitalizingService :: (Member Console r, Member Capitalize r) => Eff r ()
capitalizingService = forever $ do
    putStrLn' "Send something to capitalize..."
    l <- getLine'
    when (null l) exitSuccess'
    capitalize l >>= putStrLn'
-------------------------------------------------------------------------------

mainPure :: IO ()
mainPure = print . run
    . runConsolePureM ["cat", "fish", "dog", "bird", ""]
    $ runCapitalizeM capitalizingService

mainConsoleA :: IO ()
mainConsoleA = runM (runConsoleM (runCapitalizeM capitalizingService))
--             |     |             |              |
--      IO () -'     |             |              |
--     Eff '[IO] () -'             |              |
--          Eff '[Console, IO] () -'              |
--             Eff '[Capitalize, Console, IO] () -'

mainConsoleB :: IO ()
mainConsoleB = runM (runCapitalizeM (runConsoleM capitalizingService))
--             |     |             |              |
--      IO () -'     |             |              |
--     Eff '[IO] () -'             |              |
--       Eff '[Capitalize, IO] () -'              |
--             Eff '[Console, Capitalize, IO] () -'

main :: IO ()
main = getArgs >>= \case
    ["pure"] -> mainPure
    ["consoleA"] -> mainConsoleA
    ["consoleB"] -> mainConsoleB
    _ -> putStrLn "Bad argument. Look into source for possible values."
