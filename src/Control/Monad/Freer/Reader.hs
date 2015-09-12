{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Freer.Reader (
  Reader(..),

  ask,
  runReader',
  runReader,
  local
) where

import Control.Monad.Freer.Internal

-- ------------------------------------------------------------------------
-- The Reader monad

-- The request for a value of type e from the current environment
-- This is a GADT because the type of values
-- returned in response to a (Reader e a) request is not any a;
-- we expect in reply the value of type 'e', the value from the
-- environment. So, the return type is restricted: 'a ~ e'
data Reader e v where
  Reader :: Reader e e

-- One can also define this as
--    data Reader e v = (e ~ v) => Reader
-- and even without GADTs, using explicit coercion:
--    newtype Reader e v = Reader (e->v)
-- In the latter case, when we make the request, we make it as Reader id.
-- So, strictly speaking, GADTs are not really necessary.


-- The signature is inferred
ask :: (Member (Reader e) r) => Eff r e
ask = send Reader

-- The handler of Reader requests. The return type shows that
-- all Reader requests are fully handled.
runReader' :: Eff (Reader e ': r) w -> e -> Eff r w
runReader' m e = loop m where
 loop (Val x) = return x
 loop (E u' q) = case decomp u' of
                  Right Reader -> loop $ qApp q e
                  Left  u      -> E u (tsingleton (qComp q loop))

-- A different way of writing the above
runReader :: Eff (Reader e ': r) w -> e -> Eff r w
runReader m e = handleRelay return (\Reader k -> k e) m

-- Locally rebind the value in the dynamic environment
-- This function is like a relay; it is both an admin for Reader requests,
-- and a requestor of them
local :: forall e a r. Member (Reader e) r =>
         (e -> e) -> Eff r a -> Eff r a
local f m = do
  e0 <- ask
  let e = f e0
  -- Local signature is needed, as always with GADTs
  let h :: Reader e v -> Arr r v a -> Eff r a
      h Reader g = g e
  interpose return h m
