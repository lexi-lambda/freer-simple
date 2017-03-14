{-# LANGUAGE NoImplicitPrelude #-}
module Fresh where

import Data.Function (($), flip)
import Data.Monoid ((<>))
import System.IO (IO)
import Text.Show (show)

import Control.Monad.Freer.Fresh (evalFresh, fresh)
import Control.Monad.Freer.Trace (runTrace, trace)


traceFresh :: IO ()
traceFresh = runTrace $ flip evalFresh 0 $ do
  n <- fresh
  trace $ "Fresh " <> show n
  n' <- fresh
  trace $ "Fresh " <> show n'
{-
Fresh 0
Fresh 1
-}
