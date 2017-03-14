{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
module Console where

import Prelude (error)

import Control.Applicative (pure)
import Control.Monad ((>>=), (>>))
import Data.Either (Either(Left, Right))
import Data.Function (($), (.))
import Data.List (reverse)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (String)
import Data.Tuple (snd)
import System.Exit (exitSuccess)
import System.IO (IO, getLine, putStrLn)

import Control.Monad.Freer (Member, send, run, runM, handleRelay, handleRelayS)
import Control.Monad.Freer.Internal (Arr, Eff(Val, E), decomp, qApp, tsingleton)


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
runConsole :: Eff '[Console, IO] w -> IO w
runConsole req = runM (handleRelay pure go req)
  where
    go :: Console v -> Arr '[IO] v w -> Eff '[IO] w
    go (PutStrLn msg) q = send (putStrLn msg) >>= q
    go GetLine q = send getLine >>= q
    go ExitSuccess q = send exitSuccess >>= q

-------------------------------------------------------------------------------
                        -- Pure Interpreter Simple --
-------------------------------------------------------------------------------
runConsolePure :: [String] -> Eff '[Console] w -> [String]
runConsolePure inputs req =
    reverse . snd $ run (handleRelayS (inputs, []) (\s _ -> pure s) go req)
  where
    go :: ([String], [String])
       -> Console v
       -> (([String], [String]) -> Arr '[] v ([String], [String]))
       -> Eff '[] ([String], [String])
    go (is, os) (PutStrLn msg) q = q (is, msg : os) ()
    go (i:is, os) GetLine q = q (is, os) i
    go ([], _) GetLine _ = error "Not enough lines"
    go (_, os) ExitSuccess _ = pure ([], os)


-------------------------------------------------------------------------------
                     -- Effectful Interpreter for Deeper Stack --
-------------------------------------------------------------------------------
runConsoleM :: Member IO r => Eff (Console ': r) w -> Eff r w
runConsoleM (Val x) = pure x
runConsoleM (E u q) = case decomp u of
    Right (PutStrLn msg) -> send (putStrLn msg) >> runConsoleM (qApp q ())
    Right GetLine        -> send getLine >>=       runConsoleM . qApp q
    Right ExitSuccess    -> send exitSuccess
    Left u'              -> E u' (tsingleton (runConsoleM . qApp q))

-------------------------------------------------------------------------------
                     -- Pure Interpreter for Deeper Stack --
-------------------------------------------------------------------------------
runConsolePureM
    :: [String]
    -> Eff (Console ': r) w
    -> Eff r (Maybe w,([String],[String]))
    -- ^ (Nothing for ExitSuccess, (unconsumed input, produced output))
runConsolePureM inputs = f (inputs,[]) where
    f
        :: ([String],[String])
        -> Eff (Console ': r) w
        -> Eff r (Maybe w,([String],[String]))
    f st (Val x) = pure (Just x, st)
    f st@(is,os) (E u q) = case decomp u of
        Right (PutStrLn msg) -> f (is, msg : os) (qApp q ())
        Right GetLine        -> case is of
            x:s -> f (s,os) (qApp q x)
            []  -> error "Not enough lines"
        Right ExitSuccess    -> pure (Nothing, st)
        Left u'              -> E u' (tsingleton (f st . qApp q))
