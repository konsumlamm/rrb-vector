{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Data.RRBVector.Internal.Sorting
    ( sort
    , sortBy
    , sortOn
    ) where

import Data.Foldable (toList)
import Data.Foldable.WithIndex (ifor_)
import Data.Primitive.Array
import Data.SamSort (sortArrayBy)
import Data.Semigroup (Arg(..))

import Data.RRBVector.Internal

uninitialized :: a
uninitialized = errorWithoutStackTrace "uninitialized"

-- | \(O(n \log n)\). Sort the vector in ascending order.
-- The sort is stable, meaning the order of equal elements is preserved.
--
-- @since 0.2.2.0
sort :: (Ord a) => Vector a -> Vector a
sort = sortBy compare

-- | \(O(n \log n)\). Sort the vector in ascending order according to the specified comparison function.
-- The sort is stable, meaning the order of equal elements is preserved.
--
-- @since 0.2.2.0
sortBy :: (a -> a -> Ordering) -> Vector a -> Vector a
sortBy cmp v =
    let sortedArr = createArray (length v) uninitialized $ \arr@(MutableArray arr#) -> do
            ifor_ v (writeArray arr)
            sortArrayBy cmp arr# 0 (length v)
    in fromList . toList $ sortedArr

-- | \(O(n \log n)\). Sort the vector in ascending order by comparing the results of applying the key function to each element.
-- The sort is stable, meaning the order of equal elements is preserved.
-- @`sortOn` f@ is equivalent to @`sortBy` (`Data.Ord.comparing` f)@, but only evaluates @f@ once for each element.
--
-- @since 0.2.2.0
sortOn :: (Ord b) => (a -> b) -> Vector a -> Vector a
sortOn f v =
    let sortedArr = createArray (length v) uninitialized $ \arr@(MutableArray arr#) -> do
            ifor_ v $ \i x -> let !y = f x in writeArray arr i (Arg y x)
            sortArrayBy compare arr# 0 (length v)
    in fromList . fmap (\(Arg _ x) -> x) . toList $ sortedArr
