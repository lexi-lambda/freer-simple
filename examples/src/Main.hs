module Main (main) where

import Control.Monad (forever, when)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import System.Environment (getArgs)

import Control.Monad.Freer (Eff, Member, runM)

import Capitalize (Capitalize, capitalize, runCapitalize)
import Console
  ( Console
  , exitSuccess'
  , getLine'
  , putStrLn'
  , runConsolePure
  , runConsoleM
  )
import Coroutine ()
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
mainPure = print
    . runConsolePure ["cat", "fish", "dog", "bird", ""]
    $ runCapitalize capitalizingService

mainConsoleA :: IO ()
mainConsoleA = runM (runConsoleM (runCapitalize capitalizingService))
--             |     |            |             |
--      IO () -'     |            |             |
--     Eff '[IO] () -'            |             |
--         Eff '[Console, IO] () -'             |
--           Eff '[Capitalize, Console, IO] () -'

mainConsoleB :: IO ()
mainConsoleB = runM (runCapitalize (runConsoleM capitalizingService))
--             |     |              |           |
--      IO () -'     |              |           |
--     Eff '[IO] () -'              |           |
--        Eff '[Capitalize, IO] () -'           |
--           Eff '[Console, Capitalize, IO] () -'

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
    msg = "Usage: prog [" ++ intercalate "|" (map fst examples) ++ "]"
