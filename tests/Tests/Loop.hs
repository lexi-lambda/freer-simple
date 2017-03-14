{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Tests.Loop
  ( runFixLoop
  , runTailLoop
  , runForeverLoop
  ) where

import           Control.Concurrent  (forkIO, killThread, threadDelay)
import           Control.Monad       (forever)
import           Control.Monad.Freer
import           Data.Function       (fix)

-- | This loops forever as expected
fixLoop :: Member IO r => Eff r ()
fixLoop = fix $ \fxLoop -> do
  send $ putStrLn "fixLoop"
  fxLoop

runFixLoop :: IO ()
runFixLoop = runM fixLoop

-- | This loops as expected
tailLoop :: Member IO r => Eff r ()
tailLoop = send (putStrLn "tailLoop") >> tailLoop

runTailLoop :: IO ()
runTailLoop = runM tailLoop

-- | This <<loop>>s.
foreverLoop ::  Member IO r => Eff r ()
foreverLoop = forever $ send $ putStrLn "loop"

runForeverLoop :: IO ()
runForeverLoop = do
  tid <- forkIO $ runM foreverLoop
  threadDelay $ 10^6 * 2
  killThread tid
