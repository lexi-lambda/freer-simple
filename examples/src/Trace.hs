{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Trace (module Trace) where

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif

import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Reader (ask, runReader)
import Control.Monad.Freer.Trace (Trace, runTrace, trace)


-- Higher-order effectful function
-- The inferred type shows that the Trace affect is added to the effects
-- of r
mapMdebug:: (Show a, Member Trace r) =>
     (a -> Eff r b) -> [a] -> Eff r [b]
mapMdebug _ [] = pure []
mapMdebug f (h:t) = do
  trace $ "mapMdebug: " <> show h
  h' <- f h
  t' <- mapMdebug f t
  pure (h':t')

tMd :: IO [Int]
tMd = runTrace $ runReader (10::Int) (mapMdebug f [1..5])
 where f x = (+) <$> ask <*> pure x
{-
mapMdebug: 1
mapMdebug: 2
mapMdebug: 3
mapMdebug: 4
mapMdebug: 5
[11,12,13,14,15]
-}

-- duplicate layers
tdup :: IO ()
tdup = runTrace $ runReader (10::Int) m
 where
 m = do
     runReader (20::Int) tr
     tr
 tr = do
      v <- ask
      trace $ "Asked: " <> show (v::Int)
{-
Asked: 20
Asked: 10
-}
