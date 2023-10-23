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
