{- |
The @'Vector' a@ type is an RRB-Vector of elements of type @a@.

This module should be imported qualified, to avoid name clashes with the "Prelude".

= Performance

The worst case running time complexities are given, with \(n\) referring to the number of elements in the vector
(or \(n_1\), \(n_2\), etc. for multiple vectors). Note that all logarithms are base 16,
so the constant factor for \(O(\log n)\) operations is quite small.

= Implementation

The implementation uses Relaxed-Radix-Balanced trees, as described by

* Nicolas Stucki, [\"Turning Relaxed Radix Balanced Vector from Theory into Practice for Scala Collections\"](https://github.com/nicolasstucki/scala-rrb-vector/blob/master/documents/Master%20Thesis%20-%20Nicolas%20Stucki%20-%20Turning%20Relaxed%20Radix%20Balanced%20Vector%20from%20Theory%20into%20Practice%20for%20Scala%20Collections.pdf), January 2015.

Currently, a branching factor of 16 is used. The tree is strict in its spine, but lazy in its elements.
-}

module Data.RRBVector
    ( Vector
    -- * Construction
    , empty, singleton, fromList, replicate
    -- ** Concatenation
    , (<|), (|>), (><)
    -- * Deconstruction
    , viewl, viewr
    -- * Indexing
    , lookup, index
    , (!?), (!)
    , update
    , adjust, adjust'
    , take, drop, splitAt
    , insertAt, deleteAt
    -- * With Index
    --
    -- | Reexported from [indexed-traversable](https://hackage.haskell.org/package/indexed-traversable).
    , module Data.Foldable.WithIndex
    , module Data.Functor.WithIndex
    , module Data.Traversable.WithIndex
    -- * Transformations
    , map, reverse
    -- * Zipping and unzipping
    , zip, zipWith, unzip
    ) where

import Prelude hiding (replicate, lookup, take, drop, splitAt, map, reverse, zip, zipWith, unzip)

import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import Data.Traversable.WithIndex

import Data.RRBVector.Internal
