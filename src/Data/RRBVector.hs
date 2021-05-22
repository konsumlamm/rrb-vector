{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
= Finite vectors

The @'Vector' a@ type represents a finite vector (or dynamic array) of elements of type @a@.

The class instances are based on those for lists.

This module should be imported qualified, to avoid name clashes with the 'Prelude'.

== Performance

The worst case running time complexities are given, with /n/ referring the the number of elements in the vector.
All logarithms are base 16, which means that /O(log n)/ behaves more like /O(1)/ in practice.

== Comparison with @Data.Sequence@

[Seq](https://hackage.haskell.org/package/containers/docs/Data-Sequence.html) is a similar container from the
[containers](https://hackage.haskell.org/package/containers) package.

@Seq@ has about the same speed for splitting and concatenation, but is considerably slower for indexing and folding.
On the other hand, @Seq@ has (amortized) /O(1)/ access to the front/back, whereas it is /O(log n)/ for @Vector@.

== Warning

The length of a 'Vector' must not exceed @'maxBound' :: 'Int'@.
Violation of this condition is not detected and if the length limit is exceeded, the behaviour of the vector is undefined.

== Implementation

The implementation uses Relaxed-Radix-Balanced trees.

* Nicolas Stucki, [\"Turning Relaxed Radix Balanced Vector from Theory into Practice for Scala Collections\"](https://raw.githubusercontent.com/nicolasstucki/scala-rrb-vector/master/documents/Master%20Thesis%20-%20Nicolas%20Stucki%20-%20Turning%20Relaxed%20Radix%20Balanced%20Vector%20from%20Theory%20into%20Practice%20for%20Scala%20Collections.pdf), January 2015.
-}

{- TODO:
construction (fromFunction, replicate, ...)
viewl, viewr, ...
ifoldl, ifoldr, ifoldl', ifoldr'
imap, itraverse
reverse

Maybe:
takeWhile, dropWhile, ...
zip, zipWith, unzip, ...
-}

-- TODO: use GHC.Exts.build (https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-Exts.html#v:build)?

module Data.RRBVector
    ( Vector(..)
    -- * Construction
    , empty
    , singleton
    , fromList
    -- * Indexing
    , lookup, index
    , update
    , adjust

    , map, imap
    --, ifoldl, ifoldr, ifoldl', ifoldr'
    --, itraverse
    , (<|), (|>), (><)
    , take, drop, splitAt
    , insertAt, deleteAt
    ) where

import Control.Applicative (Alternative)
import qualified Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Monad (when, MonadPlus)
import Control.Monad.ST
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Zip (MonadZip(..))

import Data.Bits
import Data.Foldable (Foldable(..), for_)
import Data.Functor.Classes
import Data.Maybe (fromMaybe)
import GHC.Exts (IsList, IsString)
import qualified GHC.Exts
import Prelude hiding (lookup, map, take, drop, splitAt, head, last)

import Data.Primitive.PrimArray
import qualified Util.Internal.Array as A
import qualified Util.Internal.Buffer as Buffer

infixr 5 ><
infixr 5 <|
infixl 5 |>

-- TODO: #ifdef ?

-- Invariant: Children of a Balanced node are always balanced.
-- A Leaf node is considered balanced.
-- Nodes are always non-empty.
data Tree a
    = Balanced !(A.Array (Tree a))
    | Unbalanced !(A.Array (Tree a)) !(PrimArray Int)
    | Leaf !(A.Array a)

data Vector a
    = Empty
    | Root
        !Int -- size
        !Int -- shift (blockShift * height)
        !(Tree a)

instance NFData a => NFData (Tree a) where
    rnf (Balanced arr) = rnf arr
    rnf (Unbalanced arr _) = rnf arr
    rnf (Leaf arr) = rnf arr

-- The number of bits used per level.
blockShift :: Int
blockShift = 4
{-# INLINE blockShift #-}

-- The maximum size of a block.
blockSize :: Int
blockSize = 1 `shiftL` blockShift

-- The mask used to extract the index into the array.
blockMask :: Int
blockMask = blockSize - 1

up :: Int -> Int
up sh = sh + blockShift
{-# INLINE up #-}

down :: Int -> Int
down sh = sh - blockShift
{-# INLINE down #-}

radixIndex :: Int -> Int -> Int
radixIndex i sh = i `shiftR` sh .&. blockMask
{-# INLINE radixIndex #-}

relaxedRadixIndex :: PrimArray Int -> Int -> Int -> (Int, Int)
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

-- | @treeSize sh@ is the size of a tree with shift @sh@.
treeSize :: Int -> Tree a -> Int
treeSize = go 0
  where
    go acc _ (Leaf arr) = acc + length arr
    go acc _ (Unbalanced _ sizes) = acc + indexPrimArray sizes (sizeofPrimArray sizes - 1)
    go acc sh (Balanced arr) =
        let i = length arr - 1
        in go (acc + i * (1 `shiftL` sh)) (down sh) (A.index arr i)
{-# INLINE treeSize #-}

-- | @computeSizes sh@ turns an array into a tree node by computing the sizes of its subtrees.
-- @sh@ is the shift of the resulting tree.
computeSizes :: Int -> A.Array (Tree a) -> Tree a
computeSizes sh arr = runST $ do
    let len = length arr
        maxSize = 1 `shiftL` sh -- the maximum size of a subtree
    sizes <- newPrimArray len
    let loop acc isBalanced i
            | i < len =
                let subtree = A.index arr i
                    size = treeSize (down sh) subtree
                    acc' = acc + size
                    isBalanced' = isBalanced && if i == len - 1 then treeBalanced subtree else size == maxSize
                in writePrimArray sizes i acc' *> loop acc' isBalanced' (i + 1)
            | otherwise = pure isBalanced
    isBalanced <- loop 0 True 0
    if isBalanced then
        pure $ Balanced arr
    else do
        sizes <- unsafeFreezePrimArray sizes -- safe because the mutable @sizes@ isn't used afterwards
        pure $ Unbalanced arr sizes

instance Show1 Vector where
    liftShowsPrec sp sl p v = showsUnaryWith (liftShowsPrec sp sl) "fromList" p (toList v)

instance (Show a) => Show (Vector a) where
    showsPrec = showsPrec1

instance Read1 Vector where
    liftReadsPrec rp rl = readsData $ readsUnaryWith (liftReadsPrec rp rl) "fromList" fromList

instance (Read a) => Read (Vector a) where
    readsPrec = readsPrec1

instance Eq1 Vector where
    liftEq f v1 v2 = length v1 == length v2 && liftEq f (toList v1) (toList v2)

instance (Eq a) => Eq (Vector a) where
    (==) = eq1

instance Ord1 Vector where
    liftCompare f v1 v2 = liftCompare f (toList v1) (toList v2)

instance (Ord a) => Ord (Vector a) where
    compare = compare1

instance Semigroup (Vector a) where
    v1 <> v2 = v1 >< v2

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
    {-# INLINE null #-}

    length Empty = 0
    length (Root s _ _) = s
    {-# INLINE length #-}

instance Functor Vector where
    fmap = map

instance Traversable Vector where
    traverse _ Empty = pure Empty
    traverse f (Root size sh tree) = Root size sh <$> traverseTree tree
      where
        traverseTree (Balanced arr) = Balanced <$> traverse traverseTree arr
        traverseTree (Unbalanced arr sizes) = Unbalanced <$> traverse traverseTree arr <*> pure sizes
        traverseTree (Leaf arr) = Leaf <$> traverse f arr

instance Applicative Vector where
    pure = singleton
    fs <*> xs = foldl' (\acc f -> acc >< fmap f xs) empty fs

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
    mzipWith f v1 v2
        | length v1 <= length v2 = imap (\i x -> f x (index i v2)) v1
        | otherwise = imap (\i x -> f (index i v1) x) v2

instance IsList (Vector a) where
    type Item (Vector a) = a
    fromList = fromList
    toList = toList

instance (a ~ Char) => IsString (Vector a) where
    fromString = fromList

instance (NFData a) => NFData (Vector a) where
    rnf Empty = ()
    rnf (Root _ _ tree) = rnf tree


-- | /O(1)/. The empty vector.
--
-- > empty = fromList []
empty :: Vector a
empty = Empty

-- | /O(1)/. A vector with a single element.
--
-- > singleton x = fromList [x]
singleton :: a -> Vector a
singleton x = Root 1 0 (Leaf $ A.singleton x)

-- | /O(n)/. Create a new vector from a list.
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
                pure [f result]
            loop (t : ts) = do
                size <- Buffer.size buffer
                if size == blockSize then do
                    result <- Buffer.get buffer
                    Buffer.push buffer t
                    rest <- loop ts
                    pure (f result : rest)
                else do
                    Buffer.push buffer t
                    loop ts
        loop trees
    {-# INLINE nodes #-}

    iterateNodes sh trees = case nodes Balanced trees of
        [tree] -> Root (treeSize sh tree) sh tree
        trees' -> iterateNodes (up sh) trees'

-- | /O(log n)/. The element at the index or 'Nothing' if the index is out of range.
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

index :: Int -> Vector a -> a
index i = fromMaybe (error "AMT.index: index out of range") . lookup i

-- | /O(log n)/. Update the element at the index with a new element.
-- Returns the original vector if the index is out of range.
update :: Int -> a -> Vector a -> Vector a
update i x = adjust i (const x)
{-# INLINE update #-}

-- | /O(log n)/. Adjust the element at the index by applying the function to it.
-- Returns the original vector if the index is out of range.
adjust :: Int -> (a -> a) -> Vector a -> Vector a
adjust _ _ Empty = Empty
adjust i f v@(Root size sh tree)
    | i < 0 || i >= size = v  -- index out of range
    | otherwise = Root size sh (adjustTree i sh tree)
  where
    adjustTree i sh (Balanced arr) = Balanced (A.adjust arr (radixIndex i sh) (adjustTree i (down sh)))
    adjustTree i sh (Unbalanced arr sizes) =
        let (idx, subIdx) = relaxedRadixIndex sizes i sh
        in Unbalanced (A.adjust arr idx (adjustTree subIdx (down sh))) sizes
    adjustTree i _ (Leaf arr) = Leaf (A.adjust arr (i .&. blockMask) f)

map :: (a -> b) -> Vector a -> Vector b
map _ Empty = Empty
map f (Root size sh tree) = Root size sh (mapTree tree)
  where
    mapTree (Balanced arr) = Balanced (fmap mapTree arr)
    mapTree (Unbalanced arr sizes) = Unbalanced (fmap mapTree arr) sizes
    mapTree (Leaf arr) = Leaf (fmap f arr)

imap :: (Int -> a -> b) -> Vector a -> Vector b
imap = undefined -- TODO

-- | /O(log n)/. Add an element to the left end of the vector.
(<|) :: a -> Vector a -> Vector a
x <| v = singleton x >< v -- TODO: optimize?
{-# INLINE (<|) #-}

-- | /O(log n)/. Add an element to the right end of the vector.
(|>) :: Vector a -> a -> Vector a
v |> x = v >< singleton x
{-# INLINE (|>) #-}

-- | /O(log n)/.
splitAt :: Int -> Vector a -> (Vector a, Vector a)
splitAt n v = (take n v, drop n v)

-- | /O(log n)/.
insertAt :: Int -> a -> Vector a -> Vector a
insertAt i x v = let (left, right) = splitAt i v in (left |> x) >< right

-- | /O(log n)/.
deleteAt :: Int -> Vector a -> Vector a
deleteAt i v = let (left, right) = splitAt (i + 1) v in take i left >< right

-- concatenation

-- | /O(log max(n, m))/.
(><) :: Vector a -> Vector a -> Vector a
Empty >< v = v
v >< Empty = v
Root size1 sh1 tree1 >< Root size2 sh2 tree2 =
    let maxShift = max sh1 sh2
        newTree = mergeTrees tree1 sh1 tree2 sh2
    in case singleTree newTree of
        Just newTree -> Root (size1 + size2) maxShift newTree
        Nothing -> Root (size1 + size2) (up maxShift) newTree
  where
    mergeTrees (Leaf arr1) _ (Leaf arr2) _ = Balanced $
        if length arr1 == blockSize then A.from2 (Leaf arr1) (Leaf arr2)
        else if length arr1 + length arr2 <= blockSize then A.singleton (Leaf (arr1 <> arr2))
        else
            let (left, right) = A.splitAt (arr1 <> arr2) blockSize
            in A.from2 (Leaf left) (Leaf right)
    mergeTrees tree1 sh1 tree2 sh2 = case compare sh1 sh2 of
        LT ->
            let right = treeToArray tree2
                (rightHead, rightTail) = viewl right
                merged = mergeTrees tree1 sh1 rightHead (down sh2)
            in mergeRebalance sh2 A.empty (treeToArray merged) rightTail
        GT ->
            let left = treeToArray tree1
                (leftInit, leftLast) = viewr left
                merged = mergeTrees leftLast (down sh1) tree2 sh2
            in mergeRebalance sh1 leftInit (treeToArray merged) A.empty
        EQ ->
            let left = treeToArray tree1
                right = treeToArray tree2
                (leftInit, leftLast) = viewr left
                (rightHead, rightTail) = viewl right
                merged = mergeTrees leftLast (down sh1) rightHead (down sh2)
            in mergeRebalance sh1 leftInit (treeToArray merged) rightTail
      where
        viewl arr = (A.head arr, A.drop arr 1)
        viewr arr = (A.take arr (length arr - 1), A.last arr)

    -- the type annotations are necessary to compile
    mergeRebalance :: forall a. Int -> A.Array (Tree a) -> A.Array (Tree a) -> A.Array (Tree a) -> Tree a
    mergeRebalance sh left center right
        | sh == blockShift = mergeRebalance' (\(Leaf arr) -> arr) Leaf
        | otherwise = mergeRebalance' treeToArray (computeSizes (down sh))
      where
        mergeRebalance' :: (Tree a -> A.Array t) -> (A.Array t -> Tree a) -> Tree a
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
            computeSizes (up sh) <$> Buffer.get newRoot
        {-# INLINE mergeRebalance' #-}

        pushTo f from to = do
            result <- Buffer.get from
            Buffer.push to (f result)
        {-# INLINE pushTo #-}

    singleTree (Balanced arr)
        | length arr == 1 = Just (A.head arr)
    singleTree (Unbalanced arr _)
        | length arr == 1 = Just (A.head arr)
    singleTree _ = Nothing

-- splitting

-- | /O(log n)/.
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
        in Balanced (A.adjust newArr idx (takeTree i (down sh)))
    takeTree i sh (Unbalanced arr sizes) =
        let (idx, subIdx) = relaxedRadixIndex sizes i sh
            newArr = A.take arr (idx + 1)
        in computeSizes sh (A.adjust newArr idx (takeTree subIdx (down sh)))
    takeTree i _ (Leaf arr) = Leaf (A.take arr ((i .&. blockMask) + 1))

-- | /O(log n)/.
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
        in computeSizes sh (A.adjust newArr 0 (dropTree n (down sh)))
    dropTree n sh (Unbalanced arr sizes) =
        let (idx, subIdx) = relaxedRadixIndex sizes n sh
            newArr = A.drop arr idx
        in computeSizes sh (A.adjust newArr 0 (dropTree subIdx (down sh)))
    dropTree n _ (Leaf arr) = Leaf (A.drop arr (n .&. blockMask))

normalize :: Vector a -> Vector a
normalize (Root size sh (Balanced arr))
    | length arr == 1 = normalize $ Root size (down sh) (A.head arr)
normalize (Root size sh (Unbalanced arr _))
    | length arr == 1 = normalize $ Root size (down sh) (A.head arr)
normalize v = v
