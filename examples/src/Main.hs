{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import Control.Monad ((>>=), forever, when)
import Data.Function (($), (.))
import Data.List (intercalate, lookup, map, null)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String (String)
import Data.Tuple (fst)
import System.Environment (getArgs)
import System.IO (IO, print, putStrLn)

import Control.Monad.Freer (Eff, Member, run, runM)

import Capitalize (Capitalize, capitalize, runCapitalizeM)
import Console
    ( Console
    , exitSuccess'
    , getLine'
    , putStrLn'
    , runConsolePureM
    , runConsoleM
    )
import Coroutine ()
import Cut ()
import Fresh ()
import Trace ()


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

examples :: [(String, IO ())]
examples =
    [ ("pure", mainPure)
    , ("consoleA", mainConsoleA)
    , ("consoleB", mainConsoleB)
    ]

main :: IO ()
main = getArgs >>= \case
    [x] -> fromMaybe e $ lookup x examples
    _ -> e
  where
    e = putStrLn msg
    msg = "Usage: prog [" <> intercalate "|" (map fst examples) <> "]"
