module Fresh where

import Control.Monad.Freer.Fresh
import Control.Monad.Freer.Trace

--------------------------------------------------------------------------------
                             -- Tests --
--------------------------------------------------------------------------------
tfresh' :: IO ()
tfresh' = runTrace $ flip runFresh' 0 $ do
  n <- fresh
  trace $ "Fresh " ++ show n
  n' <- fresh
  trace $ "Fresh " ++ show n'
{-
Fresh 0
Fresh 1
-}
