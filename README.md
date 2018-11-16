# freer-simple — a friendly effect system for Haskell [![Build Status](https://travis-ci.org/lexi-lambda/freer-simple.svg?branch=master)](https://travis-ci.org/lexi-lambda/freer-simple)

The `freer-simple` library is an implementation of an *extensible effect system* for Haskell, a general-purpose way of tracking effects at the type level and handling them in different ways. The concept of an “effect” is very general: it encompasses the things most people consider side-effects, like generating random values, interacting with the file system, and mutating state, but it also includes things like access to an immutable global environment and exception handling.

The key features of `freer-simple` are:

  - An efficient effect system for Haskell as a library.

  - Implementations for several common Haskell monads as effects, including `Reader`, `Writer`, `State`, `Error`, and others.

  - A combinator language for defining your own effects, designed to make simple, common use cases easy to read and write.

[**For more details, see the package documentation on Hackage.**](https://hackage.haskell.org/package/freer-simple)

## Code example

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

import qualified Prelude
import qualified System.Exit

import Prelude hiding (putStrLn, getLine)

import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.Freer.Error
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer

--------------------------------------------------------------------------------
                               -- Effect Model --
--------------------------------------------------------------------------------
data Console r where
  PutStrLn    :: String -> Console ()
  GetLine     :: Console String
  ExitSuccess :: Console ()
makeEffect ''Console

--------------------------------------------------------------------------------
                          -- Effectful Interpreter --
--------------------------------------------------------------------------------
runConsole :: Eff '[Console, IO] a -> IO a
runConsole = runM . interpretM (\case
  PutStrLn msg -> Prelude.putStrLn msg
  GetLine -> Prelude.getLine
  ExitSuccess -> System.Exit.exitSuccess)

--------------------------------------------------------------------------------
                             -- Pure Interpreter --
--------------------------------------------------------------------------------
runConsolePure :: [String] -> Eff '[Console] w -> [String]
runConsolePure inputs req = snd . fst $
    run (runWriter (runState inputs (runError (reinterpret3 go req))))
  where
    go :: Console v -> Eff '[Error (), State [String], Writer [String]] v
    go (PutStrLn msg) = tell [msg]
    go GetLine = get >>= \case
      [] -> error "not enough lines"
      (x:xs) -> put xs >> pure x
    go ExitSuccess = throwError ()
```

## Acknowledgements

The `freer-simple` package began as a fork of [freer-effects](http://hackage.haskell.org/package/freer-effects) by Ixperta Solutions, which in turn is a fork of [freer](http://hackage.haskell.org/package/freer) by Allele Dev. All implementations are based on the [paper and reference implementation by Oleg Kiselyov](http://okmij.org/ftp/Haskell/extensible/more.pdf).
