# Change Log

All notable changes to this project will be documented in this file.

## [Unreleased]

* Introduced `replaceRelay` and `replaceRelayS`.
  [PR #27](https://github.com/IxpertaSolutions/freer-effects/pull/27)
  (**new**)
* Fix: `Control.Monad.forever` causes `<<loop>>`
  [#23](https://github.com/IxpertaSolutions/freer-effects/issues/23)
  (**bug-fix**)
* Switched to hpack format for package description, see [hpack
  documentation](https://github.com/sol/hpack#readme) for more details.
  Cabal file is still provided, but it is generated from `package.yaml`.
  (**change**)
* Coroutines can now return values when they are done. This affects only the
  `Done` constructor of `Status` data type.
  [PR #25](https://github.com/IxpertaSolutions/freer-effects/pull/25)
  (**breaking-change**)
* Introduced new handler `interposeC` for `Yield` effect (coroutines), and
  helper function named `replyC`.
  [PR #25](https://github.com/IxpertaSolutions/freer-effects/pull/25)
  (**new**)
* Introduced `Fresh` effect handlers `runFresh` and `evalFresh`. Function
  `runFresh'` was deprecated in favour of `evalFresh`.
  [PR #24](https://github.com/IxpertaSolutions/freer-effects/pull/24)
  (**new, change**)
* Introduced `raise` to weaken an effect stack.
  [PR #41](https://github.com/IxpertaSolutions/freer-effects/pull/41)
  (**new**)
* Added support for custom type errors for unsolvable `Member` constraints.
  [PR #48](https://github.com/IxpertaSolutions/freer-effects/pull/48)
  (**new**)

## [0.3.0.1] (April 16, 2017)

* Relax `hlint` version bounds and disable `hlint` tests in non-developer
  builds.
  [#31](https://github.com/IxpertaSolutions/freer-effects/issues/31)

## [0.3.0.0] (March 06, 2017)

* Package renamed to `freer-effects` to distinguish it from original `freer`.
  [#4](https://github.com/IxpertaSolutions/freer-effects/issues/4)
* Fix `Could not deduce: effs ~ (r : rs)` that may occur when using
  a `Member` contraint (a regression introduced in 0.2.4.0)
  [freer!12](https://gitlab.com/queertypes/freer/merge_requests/12)
* Add `runNatS` convenience function
  [freer!13](https://gitlab.com/queertypes/freer/merge_requests/13)
* Add `evalState` and `execState` convenience functions
  [freer!14](https://gitlab.com/queertypes/freer/merge_requests/14)
* Data constructors of `Yield`, `CutFalse`, `Fresh`, `State` and `Trace`
  are now exposed in addition to `Exc`, `Reader` and `Writer`
* Generalised type signature of `asks`.
  [#7](https://github.com/IxpertaSolutions/freer-effects/issues/7)
* Renamed modules `Data.Open.Union.*` to `Data.OpenUnion.*`.
  [#8](https://github.com/IxpertaSolutions/freer-effects/issues/8)
* `NonDetEff` separated into its own module and renamed to `NonDet`.
  [#11](https://github.com/IxpertaSolutions/freer-effects/issues/11)
* Reimplement `Union` using
  <http://okmij.org/ftp/Haskell/extensible/OpenUnion51.hs> as a basis.
  [#14](https://github.com/IxpertaSolutions/freer-effects/issues/14)
* Renamed `Teletype` example DSL to `Console`.

## [0.2.4.1] (November 25, 2016)

* Restore GHC (7.8, 7.10) compatibility

## 0.2.4.0 (November 25, 2016)

* Internal reorg
  * In particular, hide implementation details in Union.Internal
  * Rewrite interpreters in terms of `extract` instead of `decomp`
* Add `runNat` convenience function

## 0.2.3.0 (June 25, 2016)

* Add GHC 8 support

## 0.2.2.2 (Sep. 14, 2015)

* Use local `data Nat` for `Data.Open.Union`
  * Using GHC.TypeLits lead to overlapping instances

## 0.2.2.1 (Sep. 14, 2015)

* Document ALL THE THINGS

## 0.2.2.0 (Sep. 13, 2015)

* Add bench suite

## 0.2.1.0 (Sep. 13, 2015)

* Add test suite

## 0.2.0.2 (Sep. 12, 2015)

* Clean up language extensions per file
* Add Teletype DSL to the README

## 0.2.0.1 (Sep. 12, 2015)

* Add Teletype DSL example
* Expose `send` in public interface

## 0.2.0.0 (Sep. 12, 2015)

* Implement NonDetEff
* Separate Cut/Coroutine out from Internals
  * Partial implementation: won't compile yet
* Extract remaining examples from Internal comments

## 0.1.1.0 (Sep. 12, 2015)

* Warnings clean up
* Examples separated from primary implementation
* Initial project documentation added

## 0.1.0.0 (Sep. 12, 2015)

* Initial release

[Unreleased]: https://github.com/IxpertaSolutions/freer/compare/0.3.0.1...HEAD
[0.3.0.1]: https://github.com/IxpertaSolutions/freer/compare/0.3.0.0...0.3.0.1
[0.3.0.0]: https://github.com/IxpertaSolutions/freer/compare/0.2.4.1...0.3.0.0
[0.2.4.1]: https://github.com/IxpertaSolutions/freer/compare/0.2.4.0...0.2.4.1
