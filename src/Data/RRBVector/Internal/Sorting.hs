module Data.RRBVector.Internal.Sorting
    ( sort
    , sortBy
    , sortOn
    , unstableSort
    , unstableSortBy
    , unstableSortOn
    ) where

import Data.Sequence.Internal.Sorting hiding
    ( buildQ, buildTQ, buildIQ, buildITQ
    , foldToMaybeTree, foldToMaybeWithIndexTree
    , sort, sortBy, sortOn
    , unstableSort, unstableSortBy, unstableSortOn
    )

import Data.RRBVector.Internal
import Data.RRBVector.Internal.Array (foldrMap1, ifoldrMap1Step)

-- stable sorting

foldToMaybeWithIndexTree :: (b -> b -> b) -> (Int -> a -> b) -> Int -> Vector a -> Maybe b
foldToMaybeWithIndexTree _ _ !_ Empty = Nothing
foldToMaybeWithIndexTree (<+>) f i (Root _ sh tree) = Just (foldTree i sh tree)
  where
    foldTree !i !sh (Balanced arr) = ifoldrMap1Step i (treeSize (down sh)) (flip foldTree (down sh)) (<+>) arr
    foldTree i sh (Unbalanced arr _) = ifoldrMap1Step i (treeSize (down sh)) (flip foldTree (down sh)) (<+>) arr
    foldTree i _ (Leaf arr) = ifoldrMap1Step i (\_ -> 1) f (<+>) arr

buildIQ :: (a -> a -> Ordering) -> (Int -> a -> IndexedQueue a) -> Vector a -> Maybe (IndexedQueue a)
buildIQ cmp f = foldToMaybeWithIndexTree (mergeIQ cmp) f 0

buildITQ :: (b -> b -> Ordering) -> (Int -> a -> IndexedTaggedQueue b a) -> Vector a -> Maybe (IndexedTaggedQueue b a)
buildITQ cmp f = foldToMaybeWithIndexTree (mergeITQ cmp) f 0

-- | \(O(n \log n)\). Sort the vector in ascending order.
-- The sort is stable, meaning the order of equal elements is preserved.
--
-- If stability is not required, `unstableSort` can be slightly faster and uses less memory.
--
-- @since 0.2.2.0
sort :: (Ord a) => Vector a -> Vector a
sort = sortBy compare

-- | \(O(n \log n)\). Sort the vector in ascending order according to the specified comparison function.
-- The sort is stable, meaning the order of equal elements is preserved.
--
-- If stability is not required, `unstableSortBy` can be slightly faster and uses less memory.
--
-- @since 0.2.2.0
sortBy :: (a -> a -> Ordering) -> Vector a -> Vector a
sortBy cmp v = case buildIQ cmp (\i x -> IQ i x IQNil) v of
    Nothing -> Empty
    Just q -> fromList $ go (length v) q
  where
    go 0 _ = []
    go n q = let (q', x) = popMinIQ cmp q in x : go (n - 1) q'

-- | \(O(n \log n)\). Sort the vector in ascending order by comparing the results of applying the key function to each element.
-- The sort is stable, meaning the order of equal elements is preserved.
-- @`sortOn` f@ is equivalent to @`sortBy` (`Data.Ord.comparing` f)@, but only evaluates @f@ once for each element.
--
-- If stability is not required, `unstableSortOn` can be slightly faster and uses less memory.
--
-- @since 0.2.2.0
sortOn :: (Ord b) => (a -> b) -> Vector a -> Vector a
sortOn f v = case buildITQ compare (\i x -> ITQ i (f x) x ITQNil) v of
    Nothing -> Empty
    Just q -> fromList $ go (length v) q
  where
    go 0 _ = []
    go n q = let (q', x) = popMinITQ compare q in x : go (n - 1) q'

-- unstable sorting

foldToMaybeTree :: (b -> b -> b) -> (a -> b) -> Vector a -> Maybe b
foldToMaybeTree _ _ Empty = Nothing
foldToMaybeTree (<+>) f (Root _ _ tree) = Just (foldTree tree)
  where
    foldTree (Balanced arr) = foldrMap1 foldTree (<+>) arr
    foldTree (Unbalanced arr _) = foldrMap1 foldTree (<+>) arr
    foldTree (Leaf arr) = foldrMap1 f (<+>) arr

buildQ :: (a -> a -> Ordering) -> (a -> Queue a) -> Vector a -> Maybe (Queue a)
buildQ cmp = foldToMaybeTree (mergeQ cmp)

buildTQ :: (b -> b -> Ordering) -> (a -> TaggedQueue b a) -> Vector a -> Maybe (TaggedQueue b a)
buildTQ cmp = foldToMaybeTree (mergeTQ cmp)

-- | \(O(n \log n)\). Sort the vector in ascending order.
-- The sort is unstable, meaning the order of equal elements might not be preserved.
--
-- If stability is required, use `sort` instead.
--
-- @since 0.2.2.0
unstableSort :: (Ord a) => Vector a -> Vector a
unstableSort = unstableSortBy compare

-- | \(O(n \log n)\). Sort the vector in ascending order according to the specified comparison function.
-- The sort is unstable, meaning the order of equal elements might not be preserved.
--
-- If stability is required, use `sortBy` instead.
--
-- @since 0.2.2.0
unstableSortBy :: (a -> a -> Ordering) -> Vector a -> Vector a
unstableSortBy cmp v = case buildQ cmp (\x -> Q x Nil) v of
    Nothing -> Empty
    Just q -> fromList $ go (length v) q
  where
    go 0 _ = []
    go n q = let (q', x) = popMinQ cmp q in x : go (n - 1) q'

-- | \(O(n \log n)\). Sort the vector in ascending order by comparing the results of applying the key function to each element.
-- The sort is stable, meaning the order of equal elements is preserved.
-- @`unstableSortOn` f@ is equivalent to @`unstableSortBy` (`Data.Ord.comparing` f)@, but only evaluates @f@ once for each element.
--
-- If stability is required, use `sortOn` instead.
--
-- @since 0.2.2.0
unstableSortOn :: (Ord b) => (a -> b) -> Vector a -> Vector a
unstableSortOn f v = case buildTQ compare (\x -> TQ (f x) x TQNil) v of
    Nothing -> Empty
    Just q -> fromList $ go (length v) q
  where
    go 0 _ = []
    go n q = let (q', x) = popMinTQ compare q in x : go (n - 1) q'
