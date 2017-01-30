{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       Control.Monad.Freer.Reader
-- Description:  Reader effects, for encapsulating an environment.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  POSIX
--
-- Composable handler for 'Reader' effects. Handy for encapsulating an
-- environment with immutable state for interpreters.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Reader
    (
    -- * Reader Effect
      Reader(..)

    -- * Reader Operations
    , ask
    , asks
    , local

    -- * Reader Handlers
    , runReader

    -- * Example 1: Simple Reader Usage
    -- $simpleReaderExample

    -- * Example 2: Modifying Reader Content With @local@
    -- $localExample
    )
  where

import Control.Applicative (pure)
import Data.Functor ((<$>))

import Control.Monad.Freer.Internal
    ( Arr
    , Eff
    , Member
    , handleRelay
    , interpose
    , send
    )


-- | Represents shared immutable environment of type @(e :: *)@ which is made
-- available to effectful computation.
data Reader e v where
  Reader :: Reader e e

-- | Request a value of the environment.
ask :: Member (Reader e) effs => Eff effs e
ask = send Reader

-- | Request a value of the environment, and apply as selector\/projection
-- function to it.
asks
    :: (e -> a)
    -- ^ The selector\/projection function to be applied to the environment.
    -> Eff '[Reader e] a
asks f = f <$> ask

-- | Handler for 'Reader' effects.
runReader :: Eff (Reader e ': effs) a -> e -> Eff effs a
runReader m e = handleRelay pure (\Reader k -> k e) m


-- | Locally rebind the value in the dynamic environment.
--
-- This function is like a relay; it is both an admin for 'Reader' requests,
-- and a requestor of them.
local
    :: forall e a effs. Member (Reader e) effs
    => (e -> e)
    -> Eff effs a
    -> Eff effs a
local f m = do
    e <- f <$> ask
    let h :: Reader e v -> Arr effs v a -> Eff effs a
        h Reader k = k e
    interpose pure h m

-- $simpleReaderExample
--
-- In this example the 'Reader' effect provides access to variable bindings.
-- Bindings are a @Map@ of integer variables. The variable @count@ contains
-- number of variables in the bindings. You can see how to run a Reader effect
-- and retrieve data from it with 'runReader', how to access the Reader data
-- with 'ask' and 'asks'.
--
-- > import Control.Monad.Freer
-- > import Control.Monad.Freer.Reader
-- > import Data.Map as Map
-- > import Data.Maybe
-- >
-- > type Bindings = Map String Int
-- >
-- > -- Returns True if the "count" variable contains correct bindings size.
-- > isCountCorrect :: Bindings -> Bool
-- > isCountCorrect bindings = run $ runReader calc_isCountCorrect bindings
-- >
-- > -- The Reader effect, which implements this complicated check.
-- > calc_isCountCorrect :: Eff '[Reader Bindings] Bool
-- > calc_isCountCorrect = do
-- >     count <- asks (lookupVar "count")
-- >     bindings <- (ask :: Eff '[Reader Bindings] Bindings)
-- >     return (count == (Map.size bindings))
-- >
-- > -- The selector function to  use with 'asks'.
-- > -- Returns value of the variable with specified name.
-- > lookupVar :: String -> Bindings -> Int
-- > lookupVar name bindings = fromJust (Map.lookup name bindings)
-- >
-- > sampleBindings :: Map.Map String Int
-- > sampleBindings = Map.fromList [("count",3), ("1",1), ("b",2)]
-- >
-- > main :: IO ()
-- > main = putStrLn
-- >     $ "Count is correct for bindings " ++ show sampleBindings ++ ": "
-- >     ++ show (isCountCorrect sampleBindings)

-- $localExample
--
-- Shows how to modify 'Reader' content with 'local'.
--
-- > import Control.Monad.Freer
-- > import Control.Monad.Freer.Reader
-- >
-- > import Data.Map as Map
-- > import Data.Maybe
-- >
-- > type Bindings = Map String Int
-- >
-- > calculateContentLen :: Eff '[Reader String] Int
-- > calculateContentLen = do
-- >     content <- (ask :: Eff '[Reader String] String)
-- >     return (length content)
-- >
-- > -- Calls calculateContentLen after adding a prefix to the Reader content.
-- > calculateModifiedContentLen :: Eff '[Reader String] Int
-- > calculateModifiedContentLen = local ("Prefix " ++) calculateContentLen
-- >
-- > main :: IO ()
-- > main = do
-- >     let s = "12345";
-- >     let modifiedLen = run $ runReader calculateModifiedContentLen s;
-- >     let len = run $ runReader calculateContentLen s ;
-- >     putStrLn $ "Modified 's' length: " ++ (show modifiedLen)
-- >     putStrLn $ "Original 's' length: " ++ (show len)
