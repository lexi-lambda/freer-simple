module Fresh (module Fresh) where

import Control.Monad.Freer.Fresh (evalFresh, fresh)
import Control.Monad.Freer.Trace (runTrace, trace)
import Control.Monad.Freer

-- | Generate two fresh values.
--
-- >>> traceFresh
-- Fresh 0
-- Fresh 1
traceFresh :: IO ()
traceFresh = runM . runTrace $ evalFresh 0 $ do
  n <- fresh
  trace $ "Fresh " ++ show n
  n' <- fresh
  trace $ "Fresh " ++ show n'
