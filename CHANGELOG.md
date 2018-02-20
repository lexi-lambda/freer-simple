# 1.1.0.0 (February 20th, 2018)

- Changed the implementation of `LastMember` to avoid an issue similar to the one with `Member` fixed in 1.0.1.1 that could cause the constraint to unnecessarily fail to solve ([#6](https://github.com/lexi-lambda/freer-simple/issues/6)).
- Changed the order of the type variables in `interpretM` to be more consistent with other functions (only relevant in combination with `TypeApplications`).
- Re-exported `(~>)` from `Control.Natural` through `Control.Monad.Freer`.

# 1.0.1.1 (January 31st, 2018)

- Fixed a bug that could cause `Member` constraints to erroneously fail to solve ([#3](https://github.com/lexi-lambda/freer-simple/pull/3)).

# 1.0.1.0 (January 27th, 2018)

- Added `subsume` to `Control.Monad.Freer` for deduplicating effects.
- Added `gets` to `Control.Monad.Freer.State` ([#1](https://github.com/lexi-lambda/freer-simple/pull/1)).

# 1.0.0.0 (December 7th, 2017)

- Initial release.
