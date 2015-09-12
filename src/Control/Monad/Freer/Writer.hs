{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module Control.Monad.Freer.Writer (
  Writer(..),
  tell,
  runWriter
) where

import Control.Monad.Freer.Internal

-- ------------------------------------------------------------------------
-- The Writer monad

-- In MTL's Writer monad, the told value must have a |Monoid| type. Our
-- writer has no such constraints. If we write a |Writer|-like
-- interpreter to accumulate the told values in a monoid, it will have
-- the |Monoid o| constraint then

data Writer o x where
  Writer :: o -> Writer o ()

tell :: Member (Writer o) r => o -> Eff r ()
tell o = send $ Writer o

-- We accumulate the told data in a list, hence no Monoid constraints
-- The code is written to be simple, not optimized.
-- If performance matters, we should convert it to accumulator

runWriter :: Eff (Writer o ': r) a -> Eff r (a,[o])
runWriter = handleRelay (\x -> return (x,[]))
                  (\ (Writer o) k -> k () >>= \ (x,l) -> return (x,o:l))
