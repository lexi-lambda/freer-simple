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

import Data.Function ((&))
import System.Exit (exitSuccess)

import Control.Monad.Freer (Eff, LastMember, Member, HasLen, interpretM, reinterpret3, run, runM, send)
import Control.Monad.Freer.Error (Error, runError, throwError)
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
runConsole = runM . interpretM (\case
  PutStrLn msg -> putStrLn msg
  GetLine -> getLine
  ExitSuccess -> exitSuccess)

-------------------------------------------------------------------------------
                        -- Pure Interpreter Simple --
-------------------------------------------------------------------------------
runConsolePure :: [String] -> Eff '[Console] w -> [String]
runConsolePure inputs req = snd . fst $
    run (runWriter (runState inputs (runError (reinterpret3 go req))))
  where
    go :: Console v -> Eff '[Error (), State [String], Writer [String]] v
    go (PutStrLn msg) = tell [msg]
    go GetLine = get >>= \case
      [] -> error "not enough lines"
      (x:xs) -> put xs >> pure x
    go ExitSuccess = throwError ()

-------------------------------------------------------------------------------
                     -- Effectful Interpreter for Deeper Stack --
-------------------------------------------------------------------------------
runConsoleM :: forall effs a. (LastMember IO effs, HasLen effs)
            => Eff (Console ': effs) a -> Eff effs a
runConsoleM = interpretM $ \case
  PutStrLn msg -> putStrLn msg
  GetLine -> getLine
  ExitSuccess -> exitSuccess

-------------------------------------------------------------------------------
                     -- Pure Interpreter for Deeper Stack --
-------------------------------------------------------------------------------
runConsolePureM
  :: forall effs w
   . HasLen effs
  => [String]
  -> Eff (Console ': effs) w
  -> Eff effs (Maybe w, [String], [String])
runConsolePureM inputs req = do
    ((x, inputs'), output) <- reinterpret3 go req
      & runError & runState inputs & runWriter
    pure (either (const Nothing) Just x, inputs', output)
  where
    go :: Console v
       -> Eff (Error () ': State [String] ': Writer [String] ': effs) v
    go (PutStrLn msg) = tell [msg]
    go GetLine = get >>= \case
      [] -> error "not enough lines"
      (x:xs) -> put xs >> pure x
    go ExitSuccess = throwError ()
