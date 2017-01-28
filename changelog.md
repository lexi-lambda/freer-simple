# Change Log
All notable changes to this project will be documented in this file.

## [Unreleased]

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

[Unreleased]: https://github.com/IxpertaSolutions/freer/compare/0.2.4.1...HEAD
[0.2.4.1]: https://github.com/IxpertaSolutions/freer/compare/0.2.4.0...0.2.4.1
