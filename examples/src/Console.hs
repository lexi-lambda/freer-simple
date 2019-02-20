module Console
  ( Console
  , exitSuccess'
  , getLine'
  , putStrLn'
  , runConsole
  , runConsoleM
  , runConsolePure
  ) where

import Data.Functor.Identity
import System.Exit (exitSuccess)

import Control.Monad.Freer (Eff, LastMember, Member, run, runM, send)
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.Interpretation
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
runConsole = runM . runConsoleM

-------------------------------------------------------------------------------
                        -- Pure Interpreter Simple --
-------------------------------------------------------------------------------
runConsolePure :: [String] -> Eff '[Console] w -> [String]
runConsolePure inputs
    = snd
    . fst
    . run
    . runWriter
    . runState inputs
    . runError
    . interpret go
    . introduce4
  where
    go :: Console v -> Eff '[Error (), State [String], Writer [String], Identity] v
    go = \case
      PutStrLn msg ->  tell [msg]
      GetLine ->  get >>= \case
        [] -> error "not enough lines"
        (x:xs) -> put xs >> pure x
      ExitSuccess ->  throwError ()

-------------------------------------------------------------------------------
                     -- Effectful Interpreter for Deeper Stack --
-------------------------------------------------------------------------------
runConsoleM :: forall effs a. LastMember IO effs
            => Eff (Console ': effs) a -> Eff effs a
runConsoleM = natural @IO $ \case
  PutStrLn msg -> putStrLn msg
  GetLine -> getLine
  ExitSuccess -> exitSuccess

