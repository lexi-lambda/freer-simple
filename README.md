# Freer: Extensible Effects with Freer Monads [![Build Status](https://travis-ci.org/lexi-lambda/freer-simple.svg?branch=master)](https://travis-ci.org/lexi-lambda/freer-simple)

# Description

The `freer-simple` library (a fork of [`freer-effects`](http://hackage.haskell.org/package/freer-effects)) is an implementation of an effect system for Haskell, which is based on the work of Oleg Kiselyov et al.:

  - [Freer Monads, More Extensible Effects](http://okmij.org/ftp/Haskell/extensible/more.pdf)
  - [Reflection without Remorse](http://okmij.org/ftp/Haskell/zseq.pdf)
  - [Extensible Effects](http://okmij.org/ftp/Haskell/extensible/exteff.pdf)

Much of the implementation is a repackaging and cleaning up of the reference materials provided [here](http://okmij.org/ftp/Haskell/extensible/).

# Features

The key features of `freer-simple` are:

  - An efficient effect system for Haskell as a library.
  - Implementations for several common Haskell monads as effects:
    - `Reader`
    - `Writer`
    - `State`
    - `Trace`
    - `Error`
  - Core components for defining your own Effects.

# Example: Console DSL

Here's what using `freer-simple` looks like:

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Console where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer
import System.Exit hiding (ExitCode(ExitSuccess))

--------------------------------------------------------------------------------
                               -- Effect Model --
--------------------------------------------------------------------------------
data Console r where
  PutStrLn    :: String -> Console ()
  GetLine     :: Console String
  ExitSuccess :: Console ()

putStrLn' :: Member Console effs => String -> Eff effs ()
putStrLn' = send . PutStrLn

getLine' :: Member Console effs => Eff effs String
getLine' = send GetLine

exitSuccess' :: Member Console effs => Eff effs ()
exitSuccess' = send ExitSuccess

--------------------------------------------------------------------------------
                          -- Effectful Interpreter --
--------------------------------------------------------------------------------
runConsole :: Eff '[Console, IO] a -> IO a
runConsole = runM . interpretM (\case
  PutStrLn msg -> putStrLn msg
  GetLine -> getLine
  ExitSuccess -> exitSuccess)

--------------------------------------------------------------------------------
                             -- Pure Interpreter --
--------------------------------------------------------------------------------
data PureResult a = PureResult {
  result :: Either () a,
  interactionLog :: [String],
  remainingInputs :: [String]
} deriving Show

runConsolePure :: [String] -> Eff '[Console] w -> PureResult w
runConsolePure inputs req =
  PureResult {
    result = fst rs,
    remainingInputs = snd rs,
    interactionLog = log
  }
  where
    (rs, log) = run (runWriter (runState inputs (runError (reinterpret3 go req))))

    go :: Console v -> Eff '[Error (), State [String], Writer [String]] v
    go (PutStrLn msg) = tell [">>> " <> msg]
    go GetLine = get >>= \case
      [] -> error "insufficient input"
      (x:xs) -> tell ["<<< " <> x ] >> put xs >> pure x
    go ExitSuccess = throwError ()

--------------------------------------------------------------------------------
                             -- The Application --
--------------------------------------------------------------------------------
app :: (Member Console r) => Eff r ()
app = do
        putStrLn' "What is your name?: "
        name <- getLine'
        putStrLn' $ "Nanu Nanu " <> name <> " have a nice day !!"
        putStrLn' "Bye !!"

--------------------------------------------------------------------------------
                             -- Demos --
--------------------------------------------------------------------------------
demoEffectful :: IO ()
demoEffectful = runConsole app

demoPure :: PureResult ()
demoPure = runConsolePure
            ["Mork",
             "I need to go write a monad tutorial, nice to meet you"]
            app

demoPureFail:: PureResult ()
demoPureFail = runConsolePure [] app
```
#### Let's Run It
Output formatted for readability:
```Bash
$ stack repl
...
*Console> demoEffectful
What is your name?:
Mindy
Nanu Nanu Mindy have a nice day !!
Bye !!

*Console> demoPure
PureResult {
            result = Right (),
            interactionLog = [">>> What is your name?: ",
                              "<<< Mork",
                              ">>> Nanu Nanu Mork have a nice day !!",
                              ">>> Bye !!"
                              ],
            remainingInputs = ["I need to go write a monad tutorial, nice to meet you"]
          }

*Console> demoPureFail
PureResult {result = *** Exception: insufficient input
```

# Contributing

Contributions are welcome! Documentation, examples, code, and feedback - they all help.


## Developer Setup

The easiest way to start contributing is to install [stack](https://haskellstack.org/). Stack can install GHC/Haskell for you, and automates common developer tasks.

The key commands are:

  - `stack setup` — install required version of GHC compiler
  - `stack build` — builds project, dependencies are automatically resolved
  - `stack test` — builds project, its tests, and executes the tests
  - `stack bench` — builds project, its benchmarks, and executes the benchamks
  - `stack ghci` — start a REPL instance with a project modules loaded
  - `stack clean`
  - `stack haddock` — builds documentation

More information about `stack` can be found in its [documentation](https://haskellstack.org/).

# Licensing

This project is distributed under a BSD3 license. See the included LICENSE file for more details.

# Acknowledgements

The `freer-simple` package started as a fork of [freer-effects](http://hackage.haskell.org/package/freer-effects) by Ixperta Solutions, which in turn is a fork of [freer](http://hackage.haskell.org/package/freer) by Allele Dev. All implementations are based on the paper and reference implementation by Oleg Kiselyov. In particular:

  - `Data.OpenUnion` maps to [OpenUnion51.hs](http://okmij.org/ftp/Haskell/extensible/OpenUnion51.hs)
  - `Data.FTCQueue` maps to [FTCQueue1](http://okmij.org/ftp/Haskell/extensible/FTCQueue1.hs)
  - `Control.Monad.Freer*` maps to [Eff1.hs](http://okmij.org/ftp/Haskell/extensible/Eff1.hs)

There will be deviations from the source.
