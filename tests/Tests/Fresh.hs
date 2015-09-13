module Tests.Fresh where

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Fresh

--------------------------------------------------------------------------------
                             -- Tests --
--------------------------------------------------------------------------------
makeFresh :: Int -> Eff r Int
makeFresh n = flip runFresh' 0 (replicateM n fresh >>= (return . last))

testFresh :: Int -> Int
testFresh = run . makeFresh
