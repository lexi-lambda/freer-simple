# Freer: Extensible Effects with Freer Monads

Freer is an implementation of
["Freer Monads, More Extensible Effects"](http://okmij.org/ftp/Haskell/extensible/more.pdf). Much
of the implementation is a repackaging and cleaning up of the
reference materials provided
[here](http://okmij.org/ftp/Haskell/extensible/).

# Features

The key features of Freer are:

* An efficient effect system for Haskell as a library
* Implementations for several common Haskell monad instances:
  * Reader
  * Writer
  * State
  * StateRW: State in terms of Reader/Writer
  * Trace
  * Exception
* Core components for defining your own Effects

# Example: Teletype DSL

Here's what using Freer looks like:

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Teletype where

import Control.Monad.Freer
import Control.Monad.Freer.Internal
import System.Exit hiding (ExitSuccess)

--------------------------------------------------------------------------------
                          -- Effect Model --
--------------------------------------------------------------------------------
data Teletype s where
  PutStrLn    :: String -> Teletype ()
  GetLine     :: Teletype String
  ExitSuccess :: Teletype ()

putStrLn' :: Member Teletype r => String -> Eff r ()
putStrLn' = send . PutStrLn

getLine'  :: Member Teletype r => Eff r String
getLine' = send GetLine

exitSuccess' :: Member Teletype r => Eff r ()
exitSuccess' = send ExitSuccess

--------------------------------------------------------------------------------
                     -- Effectful Interpreter --
--------------------------------------------------------------------------------
runTeletype :: Eff '[Teletype] w -> IO w
runTeletype (Val x) = return x
runTeletype (E u q) = case extract u of
              (PutStrLn msg) -> putStrLn msg  >> runTeletype (qApp q ())
              GetLine        -> getLine      >>= \s -> runTeletype (qApp q s)
              ExitSuccess    -> exitSuccess

--------------------------------------------------------------------------------
                        -- Pure Interpreter --
--------------------------------------------------------------------------------
runTeletypePure :: [String] -> Eff '[Teletype] w -> [String]
runTeletypePure inputs req =
  reverse . snd $ run (handleRelayS (inputs, []) (\s _ -> pure s) go req)
  where
    go :: ([String], [String])
       -> Teletype v
       -> (([String], [String]) -> Arr '[] v ([String], [String]))
       -> Eff '[] ([String], [String])
    go (is, os) (PutStrLn msg) q = q (is, msg : os) ()
    go (i:is, os) GetLine q = q (is, os) i
    go ([], _) GetLine _ = error "Not enough lines"
    go (_, os) ExitSuccess _ = pure ([], os)
```

# Contributing

Contributions are welcome! Documentation, examples, code, and
feedback - they all help.

Be sure to review the included code of conduct. This project adheres
to the [Contributor's Covenant](http://contributor-covenant.org/). By
participating in this project you agree to abide by its terms.

## Developer Setup

The easiest way to start contributing is to install
[stack](https://github.com/commercialhaskell/stack). stack can install
GHC/Haskell for you, and automates common developer tasks.

The key commands are:

* stack setup : install GHC
* stack build
* stack clean
* stack haddock : builds documentation
* stack test
* stack bench
* stack ghci : start a REPL instance

# Licensing

This project is distrubted under a BSD3 license. See the included
LICENSE file for more details.

# Acknowledgements

This package would not be possible without the paper and the reference
implementation. In particular:

* Data.Open.Union maps to [OpenUnion41.hs](http://okmij.org/ftp/Haskell/extensible/OpenUnion41.hs)
* Data.FTCQueue maps to [FTCQueue1](http://okmij.org/ftp/Haskell/extensible/FTCQueue1.hs)
* Control.Monad.Freer* maps to [Union1.hs](http://okmij.org/ftp/Haskell/extensible/Eff1.hs)

There will be deviations from the source.
