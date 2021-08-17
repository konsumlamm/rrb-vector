{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.RRBVector.Internal
    ( Vector(..)
    , Tree(..)
    -- * Internal
    , blockShift, blockSize, treeSize, computeSizes, up
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
    -- * Transformations
    , map, reverse
    -- * Zipping and unzipping
    , zip, zipWith, unzip
    ) where

import Control.Applicative (Alternative, liftA2)
import qualified Control.Applicative
import Control.DeepSeq
import Control.Monad (when, MonadPlus)
import Control.Monad.ST (runST)
#if !(MIN_VERSION_base(4,13,0))
import Control.Monad.Fail (MonadFail(..))
#endif
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Zip (MonadZip(..))

import Data.Bits
import Data.Foldable (Foldable(..), for_)
import Data.Functor.Classes
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Semigroup
import qualified GHC.Exts as Exts
import GHC.Stack (HasCallStack)
import Prelude hiding (replicate, lookup, map, take, drop, splitAt, head, last, reverse, zip, zipWith, unzip)
import Text.Read

import Data.Functor.WithIndex
import Data.Foldable.WithIndex
import Data.Traversable.WithIndex

import Data.Primitive.PrimArray
import qualified Data.RRBVector.Internal.Array as A
import qualified Data.RRBVector.Internal.Buffer as Buffer
import Data.RRBVector.Internal.Indexed

infixr 5 ><
infixr 5 <|
infixl 5 |>

type Shift = Int

-- Invariant: Children of a Balanced node are always balanced.
-- A Leaf node is considered balanced.
-- Nodes are always non-empty.
data Tree a
    = Balanced {-# UNPACK #-} !(A.Array (Tree a))
    | Unbalanced {-# UNPACK #-} !(A.Array (Tree a)) !(PrimArray Int)
    | Leaf {-# UNPACK #-} !(A.Array a)

-- | A vector.
--
-- The instances are based on those of @Seq@s, which are in turn based on those of lists.
data Vector a
    = Empty
    | Root
        !Int -- size
        !Shift -- shift (blockShift * height)
        !(Tree a)

-- The number of bits used per level.
blockShift :: Shift
blockShift = 4

-- The maximum size of a block.
blockSize :: Int
blockSize = 1 `unsafeShiftL` blockShift

-- The mask used to extract the index into the array.
blockMask :: Int
blockMask = blockSize - 1

up :: Shift -> Shift
up sh = sh + blockShift

down :: Shift -> Shift
down sh = sh - blockShift

radixIndex :: Int -> Shift -> Int
radixIndex i sh = i `unsafeShiftR` sh .&. blockMask

relaxedRadixIndex :: PrimArray Int -> Int -> Shift -> (Int, Int)
relaxedRadixIndex sizes i sh =
    let guess = radixIndex i sh -- guess <= idx
        idx = loop guess
        subIdx = if idx == 0 then i else i - indexPrimArray sizes (idx - 1)
    in (idx, subIdx)
  where
    loop idx =
        let current = indexPrimArray sizes idx -- idx will always be in range for a well-formed tree
        in if i < current then idx else loop (idx + 1)

treeToArray :: Tree a -> A.Array (Tree a)
treeToArray (Balanced arr) = arr
treeToArray (Unbalanced arr _) = arr
treeToArray (Leaf _) = error "treeToArray: leaf"

treeBalanced :: Tree a -> Bool
treeBalanced (Balanced _) = True
treeBalanced (Unbalanced _ _) = False
treeBalanced (Leaf _) = True

-- @treeSize sh@ is the size of a tree with shift @sh@.
treeSize :: Shift -> Tree a -> Int
treeSize = go 0
  where
    go !acc !_ (Leaf arr) = acc + length arr
    go acc _ (Unbalanced _ sizes) = acc + indexPrimArray sizes (sizeofPrimArray sizes - 1)
    go acc sh (Balanced arr) =
        let i = length arr - 1
        in go (acc + i * (1 `unsafeShiftL` sh)) (down sh) (A.index arr i)
{-# INLINE treeSize #-}

-- @computeSizes sh@ turns an array into a tree node by computing the sizes of its subtrees.
-- @sh@ is the shift of the resulting tree.
computeSizes :: Shift -> A.Array (Tree a) -> Tree a
computeSizes !sh arr
    | isBalanced = Balanced arr
    | otherwise = runST $ do
        sizes <- newPrimArray (length arr)
        let loop acc i
                | i < len =
                    let size = treeSize (down sh) (A.index arr i)
                        acc' = acc + size
                    in writePrimArray sizes i acc' *> loop acc' (i + 1)
                | otherwise = do
                    sizes <- unsafeFreezePrimArray sizes -- safe because the mutable @sizes@ isn't used afterwards
                    pure $ Unbalanced arr sizes
        loop 0 0
  where
    maxSize = 1 `unsafeShiftL` sh -- the maximum size of a subtree

    len = length arr

    lenM1 = len - 1

    isBalanced = go 0
      where
        go i
            | i < lenM1 = treeSize (down sh) subtree == maxSize && go (i + 1)
            | otherwise = treeBalanced subtree
          where
            subtree = A.index arr i

-- Integer log base 2.
log2 :: Int -> Int
log2 x = bitSizeMinus1 - countLeadingZeros x
  where
    bitSizeMinus1 = finiteBitSize (0 :: Int) - 1

instance Show1 Vector where
    liftShowsPrec sp sl p v = showsUnaryWith (liftShowsPrec sp sl) "fromList" p (toList v)

instance (Show a) => Show (Vector a) where
    showsPrec = showsPrec1

instance Read1 Vector where
    liftReadPrec rp rl = readData $ readUnaryWith (liftReadPrec rp rl) "fromList" fromList
    liftReadListPrec = liftReadListPrecDefault

instance (Read a) => Read (Vector a) where
    readPrec = readPrec1
    readListPrec = readListPrecDefault

instance Eq1 Vector where
    liftEq f v1 v2 = length v1 == length v2 && liftEq f (toList v1) (toList v2)

instance (Eq a) => Eq (Vector a) where
    (==) = eq1

instance Ord1 Vector where
    liftCompare f v1 v2 = liftCompare f (toList v1) (toList v2)

instance (Ord a) => Ord (Vector a) where
    compare = compare1

instance Semigroup (Vector a) where
    (<>) = (><)
    stimes = stimesMonoid

instance Monoid (Vector a) where
    mempty = empty

instance Foldable Vector where
    foldr f acc = go
      where
        go Empty = acc
        go (Root _ _ tree) = foldrTree tree acc

        foldrTree (Balanced arr) acc' = foldr foldrTree acc' arr
        foldrTree (Unbalanced arr _) acc' = foldr foldrTree acc' arr
        foldrTree (Leaf arr) acc' = foldr f acc' arr
    {-# INLINE foldr #-}

    foldl f acc = go
      where
        go Empty = acc
        go (Root _ _ tree) = foldlTree acc tree

        foldlTree acc' (Balanced arr) = foldl foldlTree acc' arr
        foldlTree acc' (Unbalanced arr _) = foldl foldlTree acc' arr
        foldlTree acc' (Leaf arr) = foldl f acc' arr
    {-# INLINE foldl #-}

    foldr' f acc = go
      where
        go Empty = acc
        go (Root _ _ tree) = foldrTree' tree acc

        foldrTree' (Balanced arr) acc' = foldr' foldrTree' acc' arr
        foldrTree' (Unbalanced arr _) acc' = foldr' foldrTree' acc' arr
        foldrTree' (Leaf arr) acc' = foldr' f acc' arr
    {-# INLINE foldr' #-}

    foldl' f acc = go
      where
        go Empty = acc
        go (Root _ _ tree) = foldlTree' acc tree

        foldlTree' acc' (Balanced arr) = foldl' foldlTree' acc' arr
        foldlTree' acc' (Unbalanced arr _) = foldl' foldlTree' acc' arr
        foldlTree' acc' (Leaf arr) = foldl' f acc' arr
    {-# INLINE foldl' #-}

    null Empty = True
    null Root{} = False

    length Empty = 0
    length (Root s _ _) = s

instance FoldableWithIndex Int Vector where
    ifoldr f z0 v = foldr' (\x g !i -> f i x (g (i + 1))) (const z0) v 0
    {-# INLINE ifoldr #-}

    ifoldl f z0 v = foldl' (\g x !i -> f i (g (i - 1)) x) (const z0) v (length v - 1)
    {-# INLINE ifoldl #-}

instance Functor Vector where
    fmap = map
    x <$ v = replicate (length v) x

instance FunctorWithIndex Int Vector where
    imap f v = runIdentity $ evalIndexed (traverse (Indexed . f') v) 0
      where
        f' x !i = WithIndex (i + 1) (Identity (f i x))

instance Traversable Vector where
    traverse _ Empty = pure Empty
    traverse f (Root size sh tree) = Root size sh <$> traverseTree tree
      where
        traverseTree (Balanced arr) = Balanced <$> A.traverse' traverseTree arr
        traverseTree (Unbalanced arr sizes) = flip Unbalanced sizes <$> A.traverse' traverseTree arr
        traverseTree (Leaf arr) = Leaf <$> A.traverse f arr
    {-# INLINE traverse #-}

instance TraversableWithIndex Int Vector where
    itraverse f v = evalIndexed (traverse (Indexed . f') v) 0
      where
        f' x !i = WithIndex (i + 1) (f i x)
    {-# INLINE itraverse #-}

instance Applicative Vector where
    pure = singleton
    fs <*> xs = foldl' (\acc f -> acc >< map f xs) empty fs
    liftA2 f xs ys = foldl' (\acc x -> acc >< map (f x) ys) empty xs
    xs *> ys = foldl' (\acc _ -> acc >< ys) empty xs
    xs <* ys = foldl' (\acc x -> acc >< replicate (length ys) x) empty xs

instance Monad Vector where
    xs >>= f = foldl' (\acc x -> acc >< f x) empty xs

instance Alternative Vector where
    empty = empty
    (<|>) = (><)

instance MonadPlus Vector

instance MonadFail Vector where
    fail _ = empty

instance MonadFix Vector where
    mfix f = fromList $ fmap (\i -> let x = index i (f x) in x) [0..length (f err) - 1]
      where
        err = error "mfix for Data.RRBVector.Vector applied to strict function"

instance MonadZip Vector where
    mzipWith = zipWith
    mzip = zip
    munzip = unzip

instance Exts.IsList (Vector a) where
    type Item (Vector a) = a
    fromList = fromList
    toList = toList

instance (a ~ Char) => Exts.IsString (Vector a) where
    fromString = fromList

instance (NFData a) => NFData (Vector a) where
    rnf = rnf1

instance NFData1 Vector where
    liftRnf f = foldl' (\_ x -> f x) ()

-- | \(O(1)\). The empty vector.
--
-- > empty = fromList []
empty :: Vector a
empty = Empty

-- | \(O(1)\). A vector with a single element.
--
-- > singleton x = fromList [x]
singleton :: a -> Vector a
singleton x = Root 1 0 (Leaf $ A.singleton x)

-- | \(O(n)\). Create a new vector from a list.
fromList :: [a] -> Vector a
fromList [] = Empty
fromList [x] = singleton x
fromList ls = case nodes Leaf ls of
    [tree] -> Root (treeSize 0 tree) 0 tree -- tree is a single leaf
    ls' -> iterateNodes blockShift ls'
  where
    nodes f trees = runST $ do
        buffer <- Buffer.new blockSize
        let loop [] = do
                result <- Buffer.get buffer
                let !x = f result
                pure [x]
            loop (t : ts) = do
                size <- Buffer.size buffer
                if size == blockSize then do
                    result <- Buffer.get buffer
                    Buffer.push buffer t
                    rest <- loop ts
                    let !x = f result
                    pure (x : rest)
                else do
                    Buffer.push buffer t
                    loop ts
        loop trees
    {-# INLINE nodes #-}

    iterateNodes sh trees = case nodes Balanced trees of
        [tree] -> Root (treeSize sh tree) sh tree
        trees' -> iterateNodes (up sh) trees'

-- | \(O(\log n)\). @replicate n x@ creates a vector of length @n@ with every element set to @x@.
--
-- >>> replicate 5 42
-- fromList [42,42,42,42,42]
--
-- @since 0.1.1.0
replicate :: Int -> a -> Vector a
replicate n x
    | n <= 0 = Empty
    | n <= blockSize = Root n 0 (Leaf $ A.replicate n x)
    | otherwise = iterateNodes blockShift (Leaf $ A.replicate blockSize x) (Leaf $ A.replicate (lastIdx .&. blockMask + 1) x)
  where
    lastIdx = n - 1

    -- @full@ is a full subtree, @rest@ is the last subtree
    iterateNodes !sh !full !rest =
        let subtreesM1 = lastIdx `unsafeShiftR` sh -- the number of subtrees minus 1
            full' = Balanced $ A.replicate blockSize full
            rest' = Balanced $ A.replicateSnoc (subtreesM1 .&. blockMask) full rest
        in if subtreesM1 < blockSize then Root n sh rest' else iterateNodes (up sh) full' rest'

-- | \(O(\log n)\). The element at the index or 'Nothing' if the index is out of range.
lookup :: Int -> Vector a -> Maybe a
lookup _ Empty = Nothing
lookup i (Root size sh tree)
    | i < 0 || i >= size = Nothing  -- index out of range
    | otherwise = Just $ lookupTree i sh tree
  where
    lookupTree i sh (Balanced arr) = lookupTree i (down sh) (A.index arr (radixIndex i sh))
    lookupTree i sh (Unbalanced arr sizes) =
        let (idx, subIdx) = relaxedRadixIndex sizes i sh
        in lookupTree subIdx (down sh) (A.index arr idx)
    lookupTree i _ (Leaf arr) = A.index arr (i .&. blockMask)

-- | \(O(\log n)\). The element at the index. Calls 'error' if the index is out of range.
index :: HasCallStack => Int -> Vector a -> a
index i = fromMaybe (error "index out of range") . lookup i
{-# INLINE index #-}

-- | \(O(\log n)\). A flipped version of 'lookup'.
(!?) :: Vector a -> Int -> Maybe a
(!?) = flip lookup

-- | \(O(\log n)\). A flipped version of 'index'.
(!) :: HasCallStack => Vector a -> Int -> a
(!) = flip index

-- | \(O(\log n)\). Update the element at the index with a new element.
-- If the index is out of range, the original vector is returned.
update :: Int -> a -> Vector a -> Vector a
update _ _ Empty = Empty
update i x v@(Root size sh tree)
    | i < 0 || i >= size = v  -- index out of range
    | otherwise = Root size sh (adjustTree i sh tree)
  where
    adjustTree i sh (Balanced arr) = Balanced (A.adjust' arr (radixIndex i sh) (adjustTree i (down sh)))
    adjustTree i sh (Unbalanced arr sizes) =
        let (idx, subIdx) = relaxedRadixIndex sizes i sh
        in Unbalanced (A.adjust' arr idx (adjustTree subIdx (down sh))) sizes
    adjustTree i _ (Leaf arr) = Leaf (A.update arr (i .&. blockMask) x)

-- | \(O(\log n)\). Adjust the element at the index by applying the function to it.
-- If the index is out of range, the original vector is returned.
adjust :: Int -> (a -> a) -> Vector a -> Vector a
adjust _ _ Empty = Empty
adjust i f v@(Root size sh tree)
    | i < 0 || i >= size = v  -- index out of range
    | otherwise = Root size sh (adjustTree i sh tree)
  where
    adjustTree i sh (Balanced arr) = Balanced (A.adjust' arr (radixIndex i sh) (adjustTree i (down sh)))
    adjustTree i sh (Unbalanced arr sizes) =
        let (idx, subIdx) = relaxedRadixIndex sizes i sh
        in Unbalanced (A.adjust' arr idx (adjustTree subIdx (down sh))) sizes
    adjustTree i _ (Leaf arr) = Leaf (A.adjust arr (i .&. blockMask) f)

-- | \(O(\log n)\). Like 'adjust', but the result of the function is forced.
adjust' :: Int -> (a -> a) -> Vector a -> Vector a
adjust' _ _ Empty = Empty
adjust' i f v@(Root size sh tree)
    | i < 0 || i >= size = v  -- index out of range
    | otherwise = Root size sh (adjustTree i sh tree)
  where
    adjustTree i sh (Balanced arr) = Balanced (A.adjust' arr (radixIndex i sh) (adjustTree i (down sh)))
    adjustTree i sh (Unbalanced arr sizes) =
        let (idx, subIdx) = relaxedRadixIndex sizes i sh
        in Unbalanced (A.adjust' arr idx (adjustTree subIdx (down sh))) sizes
    adjustTree i _ (Leaf arr) = Leaf (A.adjust' arr (i .&. blockMask) f)

-- | \(O(n)\). Apply the function to every element.
--
-- >>> map (+ 1) (fromList [1, 2, 3])
-- fromList [2,3,4]
map :: (a -> b) -> Vector a -> Vector b
map _ Empty = Empty
map f (Root size sh tree) = Root size sh (mapTree tree)
  where
    mapTree (Balanced arr) = Balanced (A.map' mapTree arr)
    mapTree (Unbalanced arr sizes) = Unbalanced (A.map' mapTree arr) sizes
    mapTree (Leaf arr) = Leaf (A.map f arr)

-- | \(O(n)\). Reverse the vector.
--
-- >>> reverse (fromList [1, 2, 3])
-- fromList [3,2,1]
reverse :: Vector a -> Vector a
reverse = fromList . foldl' (flip (:)) [] -- convert the vector to a reverse list and then rebuild

-- | \(O(\min(n_1, n_2))\). Take two vectors and return a vector of corresponding pairs.
-- If one input is longer, excess elements are discarded from the right end.
--
-- > zip = zipWith (,)
zip :: Vector a -> Vector b -> Vector (a, b)
zip = zipWith (,)

-- | \(O(\min(n_1, n_2))\). 'zipWith' generalizes 'zip' by zipping with the function.
zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith f v1 v2 = fromList $ List.zipWith f (toList v1) (toList v2)

-- | \(O(n)\). Unzip a vector of pairs.
--
-- >>> unzip (fromList [(1, "a"), (2, "b"), (3, "c")])
-- (fromList [1,2,3],fromList ["a","b","c"])
unzip :: Vector (a, b) -> (Vector a, Vector b)
unzip v =
    let !left = map fst v
        !right = map snd v
    in (left, right)

-- | \(O(\log n)\). The first element and the vector without the first element, or 'Nothing' if the vector is empty.
--
-- >>> viewl (fromList [1, 2, 3])
-- Just (1,fromList [2,3])
viewl :: Vector a -> Maybe (a, Vector a)
viewl Empty = Nothing
viewl v@(Root _ _ tree) = let !tail = drop 1 v in Just (headTree tree, tail)
  where
    headTree (Balanced arr) = headTree (A.head arr)
    headTree (Unbalanced arr _) = headTree (A.head arr)
    headTree (Leaf arr) = A.head arr

-- | \(O(\log n)\). The vector without the last element and the last element, or 'Nothing' if the vector is empty.
--
-- >>> viewr (fromList [1, 2, 3])
-- Just (fromList [1,2],3)
viewr :: Vector a -> Maybe (Vector a, a)
viewr Empty = Nothing
viewr v@(Root size _ tree) = let !init = take (size - 1) v in Just (init, lastTree tree)
  where
    lastTree (Balanced arr) = lastTree (A.last arr)
    lastTree (Unbalanced arr _) = lastTree (A.last arr)
    lastTree (Leaf arr) = A.last arr

-- | \(O(\log n)\). Split the vector at the given index.
--
-- > splitAt n v = (take n v, drop n v)
splitAt :: Int -> Vector a -> (Vector a, Vector a)
splitAt n v =
    let !left = take n v
        !right = drop n v
    in (left, right)

-- | \(O(\log n)\). Insert an element at the given index.
insertAt :: Int -> a -> Vector a -> Vector a
insertAt i x v = let (left, right) = splitAt i v in (left |> x) >< right

-- | \(O(\log n)\). Delete the element at the given index.
deleteAt :: Int -> Vector a -> Vector a
deleteAt i v = let (left, right) = splitAt (i + 1) v in take i left >< right

-- concatenation

-- | \(O(\log \max(n_1, n_2))\). Concatenates two vectors.
--
-- >>> fromList [1, 2, 3] >< fromList [4, 5]
-- fromList [1,2,3,4,5]
(><) :: Vector a -> Vector a -> Vector a
Empty >< v = v
v >< Empty = v
Root size1 sh1 tree1 >< Root size2 sh2 tree2 =
    let maxShift = max sh1 sh2
        upMaxShift = up maxShift
        newArr = mergeTrees tree1 sh1 tree2 sh2
    in if length newArr == 1
        then Root (size1 + size2) maxShift (A.head newArr)
        else Root (size1 + size2) upMaxShift (computeSizes upMaxShift newArr)
  where
    mergeTrees tree1@(Leaf arr1) !_ tree2@(Leaf arr2) !_
        | length arr1 == blockSize = A.from2 tree1 tree2
        | length arr1 + length arr2 <= blockSize = A.singleton $! Leaf (arr1 <> arr2)
        | otherwise =
            let (left, right) = A.splitAt (arr1 <> arr2) blockSize
                !leftTree = Leaf left
                !rightTree = Leaf right
            in A.from2 leftTree rightTree
    mergeTrees tree1 sh1 tree2 sh2 = case compare sh1 sh2 of
        LT ->
            let !right = treeToArray tree2
                (rightHead, rightTail) = viewlArr right
                merged = mergeTrees tree1 sh1 rightHead (down sh2)
            in mergeRebalance sh2 A.empty merged rightTail
        GT ->
            let !left = treeToArray tree1
                (leftInit, leftLast) = viewrArr left
                merged = mergeTrees leftLast (down sh1) tree2 sh2
            in mergeRebalance sh1 leftInit merged A.empty
        EQ ->
            let !left = treeToArray tree1
                !right = treeToArray tree2
                (leftInit, leftLast) = viewrArr left
                (rightHead, rightTail) = viewlArr right
                merged = mergeTrees leftLast (down sh1) rightHead (down sh2)
            in mergeRebalance sh1 leftInit merged rightTail
      where
        viewlArr arr = (A.head arr, A.drop arr 1)

        viewrArr arr = (A.take arr (length arr - 1), A.last arr)

    -- the type signature is necessary to compile
    mergeRebalance :: forall a. Shift -> A.Array (Tree a) -> A.Array (Tree a) -> A.Array (Tree a) -> A.Array (Tree a)
    mergeRebalance !sh !left !center !right
        | sh == blockShift = mergeRebalance' (\(Leaf arr) -> arr) Leaf
        | otherwise = mergeRebalance' treeToArray (computeSizes (down sh))
      where
        mergeRebalance' :: (Tree a -> A.Array t) -> (A.Array t -> Tree a) -> A.Array (Tree a)
        mergeRebalance' extract construct = runST $ do
            newRoot <- Buffer.new blockSize
            newSubtree <- Buffer.new blockSize
            newNode <- Buffer.new blockSize
            for_ (toList left ++ toList center ++ toList right) $ \subtree ->
                for_ (extract subtree) $ \x -> do
                    lenNode <- Buffer.size newNode
                    when (lenNode == blockSize) $ do
                        pushTo construct newNode newSubtree
                        lenSubtree <- Buffer.size newSubtree
                        when (lenSubtree == blockSize) $ pushTo (computeSizes sh) newSubtree newRoot
                    Buffer.push newNode x
            pushTo construct newNode newSubtree
            pushTo (computeSizes sh) newSubtree newRoot
            Buffer.get newRoot
        {-# INLINE mergeRebalance' #-}

        pushTo f from to = do
            result <- Buffer.get from
            Buffer.push to $! f result
        {-# INLINE pushTo #-}

-- | \(O(\log n)\). Add an element to the left end of the vector.
--
-- >>> 1 <| fromList [2, 3, 4]
-- fromList [1,2,3,4]
(<|) :: a -> Vector a -> Vector a
x <| Empty = singleton x
x <| Root size sh tree
    | insertShift > sh = Root (size + 1) insertShift (computeSizes insertShift (let !new = newBranch x sh in A.from2 new tree))
    | otherwise = Root (size + 1) sh (consTree sh tree)
  where
    consTree sh (Balanced arr)
        | sh == insertShift = computeSizes sh (A.cons arr $! newBranch x (down sh))
        | otherwise = computeSizes sh (A.adjust' arr 0 (consTree (down sh)))
    consTree sh (Unbalanced arr _)
        | sh == insertShift = computeSizes sh (A.cons arr $! newBranch x (down sh))
        | otherwise = computeSizes sh (A.adjust' arr 0 (consTree (down sh)))
    consTree _ (Leaf arr) = Leaf $ A.cons arr x

    insertShift = computeShift size sh (up sh) tree

    -- compute the shift at which the new branch needs to be inserted (0 means there is space in the leaf)
    -- the size is computed for efficient calculation of the shift in a balanced subtree
    computeShift !sz !sh !min (Balanced _) =
        let newShift = (log2 sz `div` blockShift) * blockShift
        in if newShift > sh then min else newShift
    computeShift _ sh min (Unbalanced arr sizes) =
        let sz' = indexPrimArray sizes 0 -- the size of the first subtree
            newMin = if length arr < blockSize then sh else min
        in computeShift sz' (down sh) newMin (A.head arr)
    computeShift _ _ min (Leaf arr) = if length arr < blockSize then 0 else min

-- | \(O(\log n)\). Add an element to the right end of the vector.
--
-- >>> fromList [1, 2, 3] |> 4
-- fromList [1,2,3,4]
(|>) :: Vector a -> a -> Vector a
Empty |> x = singleton x
Root size sh tree |> x
    | insertShift > sh = Root (size + 1) insertShift (computeSizes insertShift (A.from2 tree $! newBranch x sh))
    | otherwise = Root (size + 1) sh (snocTree sh tree)
  where
    snocTree sh (Balanced arr)
        | sh == insertShift = Balanced (A.snoc arr $! newBranch x (down sh)) -- the current subtree is fully balanced
        | otherwise = Balanced $ A.adjust' arr (length arr - 1) (snocTree (down sh))
    snocTree sh (Unbalanced arr sizes)
        | sh == insertShift = Unbalanced (A.snoc arr $! newBranch x (down sh)) newSizesSnoc
        | otherwise = Unbalanced (A.adjust' arr (length arr - 1) (snocTree (down sh))) newSizesAdjust
      where
        -- snoc the last size + 1
        newSizesSnoc = runST $ do
            let lenSizes = sizeofPrimArray sizes
            newArr <- newPrimArray (lenSizes + 1)
            copyPrimArray newArr 0 sizes 0 lenSizes
            let lastSize = indexPrimArray sizes (lenSizes - 1)
            writePrimArray newArr lenSizes (lastSize + 1)
            unsafeFreezePrimArray newArr
        -- adjust the last size with (+ 1)
        newSizesAdjust = runST $ do
            let lenSizes = sizeofPrimArray sizes
            newArr <- newPrimArray lenSizes
            copyPrimArray newArr 0 sizes 0 lenSizes
            let lastSize = indexPrimArray sizes (lenSizes - 1)
            writePrimArray newArr (lenSizes - 1) (lastSize + 1)
            unsafeFreezePrimArray newArr
    snocTree _ (Leaf arr) = Leaf $ A.snoc arr x

    insertShift = computeShift size sh (up sh) tree

    -- compute the shift at which the new branch needs to be inserted (0 means there is space in the leaf)
    -- the size is computed for efficient calculation of the shift in a balanced subtree
    computeShift !sz !sh !min (Balanced _) =
        let newShift = (countTrailingZeros sz `div` blockShift) * blockShift
        in if newShift > sh then min else newShift
    computeShift _ sh min (Unbalanced arr sizes) =
        let lastIdx = sizeofPrimArray sizes - 1
            sz' = indexPrimArray sizes lastIdx - indexPrimArray sizes (lastIdx - 1) -- the size of the last subtree
            newMin = if length arr < blockSize then sh else min
        in computeShift sz' (down sh) newMin (A.last arr)
    computeShift _ _ min (Leaf arr) = if length arr < blockSize then 0 else min

-- create a new tree with shift @sh@
newBranch :: a -> Shift -> Tree a
newBranch x = go
  where
    go 0 = Leaf (A.singleton x)
    go sh = Balanced (A.singleton $! go (down sh))

-- splitting

-- | \(O(\log n)\). The first @i@ elements of the vector.
-- If @i@ is negative, the empty vector is returned. If the vector contains less than @i@ elements, the whole vector is returned.
take :: Int -> Vector a -> Vector a
take _ Empty = Empty
take n v@(Root size sh tree)
    | n <= 0 = empty
    | n >= size = v
    | otherwise = normalize $ Root n sh (takeTree (n - 1) sh tree)
  where
    -- the initial @i@ is @n - 1@ -- the index of the last element in the new tree
    takeTree i sh (Balanced arr) =
        let idx = radixIndex i sh
            newArr = A.take arr (idx + 1)
        in Balanced (A.adjust' newArr idx (takeTree i (down sh)))
    takeTree i sh (Unbalanced arr sizes) =
        let (idx, subIdx) = relaxedRadixIndex sizes i sh
            newArr = A.take arr (idx + 1)
        in computeSizes sh (A.adjust' newArr idx (takeTree subIdx (down sh)))
    takeTree i _ (Leaf arr) = Leaf (A.take arr ((i .&. blockMask) + 1))

-- | \(O(\log n)\). The vector without the first @i@ elements
-- If @i@ is negative, the whole vector is returned. If the vector contains less than @i@ elements, the empty vector is returned.
drop :: Int -> Vector a -> Vector a
drop _ Empty = Empty
drop n v@(Root size sh tree)
    | n <= 0 = v
    | n >= size = empty
    | otherwise = normalize $ Root (size - n) sh (dropTree n sh tree)
  where
    dropTree n sh (Balanced arr) =
        let idx = radixIndex n sh
            newArr = A.drop arr idx
        in computeSizes sh (A.adjust' newArr 0 (dropTree n (down sh)))
    dropTree n sh (Unbalanced arr sizes) =
        let (idx, subIdx) = relaxedRadixIndex sizes n sh
            newArr = A.drop arr idx
        in computeSizes sh (A.adjust' newArr 0 (dropTree subIdx (down sh)))
    dropTree n _ (Leaf arr) = Leaf (A.drop arr (n .&. blockMask))

normalize :: Vector a -> Vector a
normalize (Root size sh (Balanced arr))
    | length arr == 1 = normalize $ Root size (down sh) (A.head arr)
normalize (Root size sh (Unbalanced arr _))
    | length arr == 1 = normalize $ Root size (down sh) (A.head arr)
normalize v = v
