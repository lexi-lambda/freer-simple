-- |
-- Module:       Control.Monad.Freer.Reader
-- Description:  Reader effects, for encapsulating an environment.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Composable handler for 'Reader' effects. Handy for encapsulating an
-- environment with immutable state for interpreters.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Reader
  ( -- * Reader Effect
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
  ) where

import Control.Monad.Freer (Eff, Member, interpose, interpret, send)

-- | Represents shared immutable environment of type @(e :: *)@ which is made
-- available to effectful computation.
data Reader r a where
  Ask :: Reader r r

-- | Request a value of the environment.
ask :: forall r effs. Member (Reader r) effs => Eff effs r
ask = send Ask

-- | Request a value of the environment, and apply as selector\/projection
-- function to it.
asks
  :: forall r effs a
   . Member (Reader r) effs
  => (r -> a)
  -- ^ The selector\/projection function to be applied to the environment.
  -> Eff effs a
asks f = f <$> ask

-- | Handler for 'Reader' effects.
runReader :: forall r effs a. r -> Eff (Reader r ': effs) a -> Eff effs a
runReader r = interpret (\Ask -> pure r)

-- | Locally rebind the value in the dynamic environment.
--
-- This function is like a relay; it is both an admin for 'Reader' requests,
-- and a requestor of them.
local
  :: forall r effs a. Member (Reader r) effs
  => (r -> r)
  -> Eff effs a
  -> Eff effs a
local f m = do
  r <- asks f
  interpose @(Reader r) (\Ask -> pure r) m

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
-- > isCountCorrect bindings = run $ runReader bindings calc_isCountCorrect
-- >
-- > -- The Reader effect, which implements this complicated check.
-- > calc_isCountCorrect :: Eff '[Reader Bindings] Bool
-- > calc_isCountCorrect = do
-- >   count <- asks (lookupVar "count")
-- >   bindings <- (ask :: Eff '[Reader Bindings] Bindings)
-- >   return (count == (Map.size bindings))
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
-- >   $ "Count is correct for bindings " ++ show sampleBindings ++ ": "
-- >   ++ show (isCountCorrect sampleBindings)

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
-- >   content <- (ask :: Eff '[Reader String] String)
-- >   return (length content)
-- >
-- > -- Calls calculateContentLen after adding a prefix to the Reader content.
-- > calculateModifiedContentLen :: Eff '[Reader String] Int
-- > calculateModifiedContentLen = local ("Prefix " ++) calculateContentLen
-- >
-- > main :: IO ()
-- > main = do
-- >   let s = "12345"
-- >   let modifiedLen = run $ runReader s calculateModifiedContentLen
-- >   let len = run $ runReader s calculateContentLen
-- >   putStrLn $ "Modified 's' length: " ++ (show modifiedLen)
-- >   putStrLn $ "Original 's' length: " ++ (show len)
