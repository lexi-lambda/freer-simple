{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
module Teletype where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure)
#endif
import System.Exit hiding (ExitSuccess)

import Control.Monad.Freer
import Control.Monad.Freer.Internal

-------------------------------------------------------------------------------
                          -- Effect Model --
-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
                     -- Effectful Interpreter Simple --
-------------------------------------------------------------------------------
runTeletype :: Eff '[Teletype, IO] w -> IO w
runTeletype req = runM (handleRelay pure go req)
  where
   go :: Teletype v -> Arr '[IO] v w -> Eff '[IO] w
   go (PutStrLn msg) q = send (putStrLn msg) >>= q
   go GetLine q = send getLine >>= q
   go ExitSuccess q = send exitSuccess >>= q

-------------------------------------------------------------------------------
                        -- Pure Interpreter Simple --
-------------------------------------------------------------------------------
runTeletypePure :: [String] -> Eff '[Teletype] w -> [String]
runTeletypePure inputs req =
  reverse . snd $ run (handleRelayS (inputs, []) (\s _ -> pure s) go req)
  where
    go :: ([String], [String])
       -> Teletype v
       -> (([String], [String]) -> Arr '[] v ([String], [String]))
       -> Eff '[] ([String], [String])
    go (is, os) (PutStrLn msg) q = q (is, msg : os) ()
    go (i:is, os) GetLine q = q (is, os) i
    go ([], _) GetLine _ = error "Not enough lines"
    go (_, os) ExitSuccess _ = pure ([], os)


-------------------------------------------------------------------------------
                     -- Effectful Interpreter for Deeper Stack --
-------------------------------------------------------------------------------
runTeletypeM :: Member IO r => Eff (Teletype ': r) w -> Eff r w
runTeletypeM (Val x) = return x
runTeletypeM (E u q) = case decomp u of
    Right (PutStrLn msg) -> send (putStrLn msg) >> runTeletypeM (qApp q ())
    Right GetLine        -> send getLine >>= \s -> runTeletypeM (qApp q s)
    Right ExitSuccess    -> send exitSuccess
    Left u'              -> E u' (tsingleton (\s -> runTeletypeM (qApp q s)))

-------------------------------------------------------------------------------
                     -- Pure Interpreter for Deeper Stack --
-------------------------------------------------------------------------------
runTeletypePureM
    :: [String]
    -> Eff (Teletype ': r) w
    -> Eff r (Maybe w,([String],[String]))
    -- ^ (Nothing for ExitSuccess, (unconsumed input, produced output))
runTeletypePureM inputs = f (inputs,[]) where
    f
        :: ([String],[String])
        -> Eff (Teletype ': r) w
        -> Eff r (Maybe w,([String],[String]))
    f st (Val x) = return (Just x, st)
    f st@(is,os) (E u q) = case decomp u of
        Right (PutStrLn msg) -> f (is, msg : os) (qApp q ())
        Right GetLine        -> case is of
            x:s -> f (s,os) (qApp q x)
            []  -> error "Not enough lines"
        Right ExitSuccess    -> pure (Nothing, st)
        Left u'              -> E u' (tsingleton (\s -> f st (qApp q s)))
