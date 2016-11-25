{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
module Teletype where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (pure)
#endif
import System.Exit hiding (ExitSuccess)

import Control.Monad.Freer

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
runTeletype :: Eff '[Teletype, IO] w -> IO w
runTeletype req = runM (handleRelay pure go req)
  where
   go :: Teletype v -> Arr '[IO] v w -> Eff '[IO] w
   go (PutStrLn msg) q = send (putStrLn msg) >>= q
   go GetLine q = send getLine >>= q
   go ExitSuccess q = send exitSuccess >>= q

--------------------------------------------------------------------------------
                        -- Pure Interpreter --
--------------------------------------------------------------------------------
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
