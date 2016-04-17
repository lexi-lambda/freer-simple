{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Control.Monad.Freer.Reader
Description : Reader effects, for encapsulating an environment
Copyright   : Alej Cabrera 2015
License     : BSD-3
Maintainer  : cpp.cabrera@gmail.com
Stability   : experimental
Portability : POSIX

Composable handler for Reader effects. Handy for encapsulating an
environment with immutable state for interpreters.

Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a
starting point.

-}
module Control.Monad.Freer.Reader (
  Reader(..),

  ask,
  asks,
  runReader,
  local
  -- * Example 1: Simple Reader Usage
  -- $simpleReaderExample

  -- * Example 2: Modifying Reader Content With @local@
  -- $localExample

) where

import Control.Monad.Freer.Internal

-- |
data Reader e v where
  Reader :: Reader e e

-- | Request a value for the environment
ask :: (Member (Reader e) r) => Eff r e
ask = send Reader

-- | Request a value from the environment and applys as function
asks :: (b -> a) -> Eff '[Reader b] a
asks f = ask >>= return . f

-- | Handler for reader effects
runReader :: Eff (Reader e ': r) w -> e -> Eff r w
runReader m e = handleRelay return (\Reader k -> k e) m

-- |
-- Locally rebind the value in the dynamic environment
-- This function is like a relay; it is both an admin for Reader requests,
-- and a requestor of them
local :: forall e a r. Member (Reader e) r =>
         (e -> e) -> Eff r a -> Eff r a
local f m = do
  e0 <- ask
  let e = f e0
  let h :: Reader e v -> Arr r v a -> Eff r a
      h Reader g = g e
  interpose return h m


{- $simpleReaderExample

In this example the @Reader@ monad provides access to variable bindings.
Bindings are a @Map@ of integer variables.
The variable @count@ contains number of variables in the bindings.
You can see how to run a Reader effect and retrieve data from it
with 'runReader', how to access the Reader data with 'ask' and 'asks'.

>import Control.Monad.Freer
>import Control.Monad.Freer.Reader
>import Data.Map as Map
>import Data.Maybe
>
>type Bindings = Map String Int
>
>asks :: (b -> a) -> Eff '[Reader b] a
>asks f = ask >>= return . f
>
>-- Returns True if the "count" variable contains correct bindings size.
>isCountCorrect :: Bindings -> Bool
>isCountCorrect bindings = run $ runReader calc_isCountCorrect bindings
>
>-- The Reader effect, which implements this complicated check.
>calc_isCountCorrect :: Eff '[Reader Bindings] Bool
>calc_isCountCorrect = do
>    count <- asks (lookupVar "count")
>    bindings <- (ask :: Eff '[Reader Bindings] Bindings)
>    return (count == (Map.size bindings))
>
>-- The selector function to  use with 'asks'.
>-- Returns value of the variable with specified name.
>lookupVar :: String -> Bindings -> Int
>lookupVar name bindings = fromJust (Map.lookup name bindings)
>
>sampleBindings :: Map.Map String Int
>sampleBindings = Map.fromList [("count",3), ("1",1), ("b",2)]
>
>main = do
>    putStr $ "Count is correct for bindings " ++ (show sampleBindings) ++ ": "
>    putStrLn $ show (isCountCorrect sampleBindings)
-}

{- $localExample

Shows how to modify Reader content with 'local'.

> import Control.Monad.Freer
> import Control.Monad.Freer.Reader
>
> import Data.Map as Map
> import Data.Maybe
>
> type Bindings = Map String Int
>
> asks :: (b -> a) -> Eff '[Reader b] a
> asks f = ask >>= return . f
>
> calculateContentLen :: Eff '[Reader String] Int
> calculateContentLen = do
>     content <- (ask :: Eff '[Reader String] String)
>     return (length content)
>
> -- Calls calculateContentLen after adding a prefix to the Reader content.
> calculateModifiedContentLen :: Eff '[Reader String] Int
> calculateModifiedContentLen = local ("Prefix " ++) calculateContentLen
>
> main :: IO ()
> main = do
>     let s = "12345";
>     let modifiedLen = run $ runReader calculateModifiedContentLen s;
>     let len = run $ runReader calculateContentLen s ;
>     putStrLn $ "Modified 's' length: " ++ (show modifiedLen)
>     putStrLn $ "Original 's' length: " ++ (show len)
-}
