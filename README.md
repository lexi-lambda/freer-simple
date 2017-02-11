# Freer Effects: Extensible Effects with Freer Monads

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)](http://www.haskell.org)
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29)

[![Hackage](http://img.shields.io/hackage/v/freer-effects.svg)](https://hackage.haskell.org/package/freer)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/freer-effects.svg)](http://packdeps.haskellers.com/reverse/freer)
[![Build](https://travis-ci.org/IxpertaSolutions/freer-effects.svg?branch=master)](https://travis-ci.org/IxpertaSolutions/freer)


# Description

Library `freer-effects` is an implementation of effect system for Haskell,
which is based on the work of Oleg Kiselyov et al.:

* [Freer Monads, More Extensible Effects](http://okmij.org/ftp/Haskell/extensible/more.pdf)
* [Reflection without Remorse](http://okmij.org/ftp/Haskell/zseq.pdf)
* [Extensible Effects](http://okmij.org/ftp/Haskell/extensible/exteff.pdf)

Much of the implementation is a repackaging and cleaning up of the reference
materials provided [here](http://okmij.org/ftp/Haskell/extensible/).


# Features

The key features of Freer are:

* An efficient effect system for Haskell as a library.
* Implementations for several common Haskell monads as effects:
  * `Reader`
  * `Writer`
  * `State`
  * `StateRW`: State in terms of Reader/Writer.
  * `Trace`
  * `Exception`
* Core components for defining your own Effects.


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

Contributions are welcome! Documentation, examples, code, and feedback - they
all help.


## Developer Setup

The easiest way to start contributing is to install
[stack](https://github.com/commercialhaskell/stack). stack can install
GHC/Haskell for you, and automates common developer tasks.

The key commands are:

* `stack setup`: install GHC
* `stack build`
* `stack clean`
* `stack haddock`: builds documentation
* `stack test`
* `stack bench`
* `stack ghci`: start a REPL instance


# Licensing

This project is distrubted under a BSD3 license. See the included
LICENSE file for more details.


# Acknowledgements

Package `freer-effects` started as a fork of
[freer](http://hackage.haskell.org/package/freer) authored by Allele Dev.

This package would not be possible without the paper and the reference
implementation. In particular:

* `Data.Open.Union` maps to
  [OpenUnion41.hs](http://okmij.org/ftp/Haskell/extensible/OpenUnion41.hs)
* `Data.FTCQueue` maps to
  [FTCQueue1](http://okmij.org/ftp/Haskell/extensible/FTCQueue1.hs)
* `Control.Monad.Freer*` maps to
  [Union1.hs](http://okmij.org/ftp/Haskell/extensible/Eff1.hs)

There will be deviations from the source.
