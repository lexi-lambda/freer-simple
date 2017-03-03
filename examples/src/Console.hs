{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
module Console where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure)
#endif
import System.Exit hiding (ExitSuccess)

import Control.Monad.Freer
import Control.Monad.Freer.Internal

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
runConsoleM (Val x) = return x
runConsoleM (E u q) = case decomp u of
    Right (PutStrLn msg) -> send (putStrLn msg) >> runConsoleM (qApp q ())
    Right GetLine        -> send getLine >>= \s -> runConsoleM (qApp q s)
    Right ExitSuccess    -> send exitSuccess
    Left u'              -> E u' (tsingleton (\s -> runConsoleM (qApp q s)))

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
    f st (Val x) = return (Just x, st)
    f st@(is,os) (E u q) = case decomp u of
        Right (PutStrLn msg) -> f (is, msg : os) (qApp q ())
        Right GetLine        -> case is of
            x:s -> f (s,os) (qApp q x)
            []  -> error "Not enough lines"
        Right ExitSuccess    -> pure (Nothing, st)
        Left u'              -> E u' (tsingleton (\s -> f st (qApp q s)))
