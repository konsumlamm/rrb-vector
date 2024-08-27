# 0.2.2.1 - July 2024

* Add `sort`, `sortBy`, `sortOn` ([#23](https://github.com/konsumlamm/rrb-vector/pull/23))
* Fix bug in `><` ([#19](https://github.com/konsumlamm/rrb-vector/pull/19))
* Fix bug in `<|` ([#18](https://github.com/konsumlamm/rrb-vector/pull/18))

# 0.2.1.0 - December 2023

* Add `findIndexL`, `findIndexR`, `findIndicesL`, `findIndicesR`
* Fix bug in `|>` & `<|` ([#10](https://github.com/konsumlamm/rrb-vector/issues/10))

# 0.2.0.1 - October 2023

* Support `primitive-0.9` and `deepseq-1.5`

# 0.2.0.0 - February 2023

* Add `map'` & `unzipWith`
* Make `zip` & `unzip` more strict
* Make all functions strict in the index
* Make the `FoldableWithIndex`, `FunctorWithIndex`, `TraversableWithIndex` instances more efficient
* Implement `ifoldMap` in the `FoldableWithIndex` instance
* Implement `fail` in the `Monad` instance
* Add `Shift`, `Tree` and unidirectional pattern synonyms for `Vector` and `Tree` to `Data.RRBVector.Internal.Debug`
* Support `primitive-0.8`

* Test typeclass laws

# 0.1.1.0 - August 2021

* Add `replicate`
* Various optimizations

* Switch to the [`tasty` framework](https://hackage.haskell.org/package/tasty) for benchmarks and tests
* Extend the test suite
  - Add properties for more functions and some typeclass instances
  - Add strictness tests

# 0.1.0.0 - June 2021

* Initial release
