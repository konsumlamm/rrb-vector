{-# LANGUAGE PatternSynonyms #-}

{- |
This module contains some debug utilities. It should only be used for debugging/testing purposes.
-}

module Data.RRBVector.Internal.Debug
    ( showTree
    , fromListUnbalanced
    , pattern Empty, pattern Root
    , Tree, Shift
    , pattern Balanced, pattern Unbalanced, pattern Leaf
    , Invariant, valid
    ) where

import Control.Monad.ST (runST)
import Data.Bits (shiftL)
import Data.Foldable (foldl', toList, traverse_)
import Data.List (intercalate)
import Data.Primitive.PrimArray (PrimArray, primArrayToList, indexPrimArray, sizeofPrimArray)

import Data.RRBVector.Internal hiding (Empty, Root, Balanced, Unbalanced, Leaf)
import qualified Data.RRBVector.Internal as RRB
import Data.RRBVector.Internal.Array (Array)
import qualified Data.RRBVector.Internal.Array as A
import qualified Data.RRBVector.Internal.Buffer as Buffer

-- | \(O(n)\). Show the underlying tree of a vector.
showTree :: (Show a) => Vector a -> String
showTree Empty = "Empty"
showTree (Root size sh tree) = "Root {size = " ++ show size ++ ", shift = " ++ show sh ++ ", tree = " ++ debugShowTree tree ++ "}"
  where
    debugShowTree (Balanced arr) = "Balanced " ++ debugShowArray arr
    debugShowTree (Unbalanced arr sizes) = "Unbalanced " ++ debugShowArray arr ++ " (" ++ show (primArrayToList sizes) ++ ")"
    debugShowTree (Leaf arr) = "Leaf " ++ show (toList arr)

    debugShowArray arr = "[" ++ intercalate "," (fmap debugShowTree (toList arr)) ++ "]"

-- | \(O(n)\). Create a new unbalanced vector from a list.
--
-- Note that it is not possbible to create an invalid 'Vector' with this function.
fromListUnbalanced :: [a] -> Vector a
fromListUnbalanced [] = RRB.Empty
fromListUnbalanced [x] = singleton x
fromListUnbalanced ls = case nodes RRB.Leaf ls of
    [tree] -> RRB.Root (treeSize 0 tree) 0 tree -- tree is a single leaf
    ls' -> iterateNodes blockShift ls'
  where
    n = blockSize - 1

    nodes f trees = runST $ do
        buffer <- Buffer.new n
        let loop [] = do
                result <- Buffer.get buffer
                pure [f result]
            loop (t : ts) = do
                size <- Buffer.size buffer
                if size == n then do
                    result <- Buffer.get buffer
                    Buffer.push buffer t
                    rest <- loop ts
                    pure (f result : rest)
                else do
                    Buffer.push buffer t
                    loop ts
        loop trees
    {-# INLINE nodes #-}

    iterateNodes sh trees = case nodes (computeSizes sh) trees of
        [tree] -> RRB.Root (treeSize sh tree) sh tree
        trees' -> iterateNodes (up sh) trees'

pattern Empty :: Vector a
pattern Empty <- RRB.Empty

pattern Root :: Int -> Shift -> Tree a -> Vector a
pattern Root size sh tree <- RRB.Root size sh tree

{-# COMPLETE Empty, Root #-}

pattern Balanced :: Array (Tree a) -> Tree a
pattern Balanced arr <- RRB.Balanced arr

pattern Unbalanced :: Array (Tree a) -> PrimArray Int -> Tree a
pattern Unbalanced arr sizes <- RRB.Unbalanced arr sizes

pattern Leaf :: Array a -> Tree a
pattern Leaf arr <- RRB.Leaf arr

{-# COMPLETE Balanced, Unbalanced, Leaf #-}

-- | Structural invariants a vector is expected to hold.
data Invariant
    = RootSizeGt0      -- Root: Size > 0
    | RootShiftDiv     -- Root: The shift at the root is divisible by blockShift
    | RootSizeCorrect  -- Root: The size at the root is correct
    | RootGt1Child     -- Root: The root has more than 1 child if not a Leaf
    | BalShiftGt0      -- Balanced: Shift > 0
    | BalNumChildren   -- Balanced: The number of children is blockSize unless
                       -- the parent is unbalanced or the node is on the right
                       -- edge in which case it is in [1,blockSize]
    | BalFullChildren  -- Balanced: All children are full, except for the last
                       -- if the node is on the right edge
    | UnbalShiftGt0    -- Unbalanced: Shift > 0
    | UnbalParentUnbal -- Unbalanced: Parent is Unbalanced
    | UnbalNumChildren -- Unbalanced: The number of children is in [1,blockSize]
    | UnbalSizes       -- Unbalanced: The sizes array is correct
    | UnbalNotBal      -- Unbalanced: The tree is not full enough to be a
                       -- Balanced
    | LeafShift0       -- Leaf: Shift == 0
    | LeafNumElems     -- Leaf: The number of elements is in [1,blockSize]
    deriving Show

assert :: Invariant -> Bool -> Either Invariant ()
assert i False = Left i
assert _ True = pure ()

-- | Check tree invariants. Returns @Left@ on finding a violated invariant.
valid :: Vector a -> Either Invariant ()
valid RRB.Empty = pure ()
valid (RRB.Root size sh tree) = do
    assert RootSizeGt0 $ size > 0
    assert RootShiftDiv $ sh `mod` blockShift == 0
    assert RootSizeCorrect $ size == countElems tree
    assert RootGt1Child $ case tree of
        Balanced arr -> length arr > 1
        Unbalanced arr _ -> length arr > 1
        Leaf _ -> True
    validTree Unbal sh tree

data NodeDesc
    = Bal           -- parent is Balanced
    | BalRightEdge  -- parent is Balanced and this node is on the right edge
    | Unbal         -- parent is Unbalanced

validTree :: NodeDesc -> Shift -> Tree a -> Either Invariant ()
validTree desc sh (RRB.Balanced arr) = do
    assert BalShiftGt0 $ sh > 0
    assert BalNumChildren $ case desc of
        Bal -> n == blockSize
        BalRightEdge -> n >= 1 && n <= blockSize
        Unbal -> n >= 1 && n <= blockSize
    assert BalFullChildren $
        all (\t -> countElems t == 1 `shiftL` sh) expectedFullChildren
    traverse_ (validTree Bal (down sh)) arrInit
    validTree descLast (down sh) (A.last arr)
  where
    n = length arr
    arrInit = A.take arr (n-1)
    expectedFullChildren = case desc of
        Bal -> arr
        BalRightEdge -> arrInit
        Unbal -> arrInit
    descLast = case desc of
        Bal -> Bal
        BalRightEdge -> BalRightEdge
        Unbal -> BalRightEdge
validTree desc sh (RRB.Unbalanced arr sizes) = do
    assert UnbalShiftGt0 $ sh > 0
    case desc of
        Bal -> assert UnbalParentUnbal False
        BalRightEdge -> assert UnbalParentUnbal False
        Unbal -> assert UnbalNumChildren $ n >= 1 && n <= blockSize
    assert UnbalSizes $ n == sizeofPrimArray sizes
    assert UnbalSizes $
        all (\i -> countElems (A.index arr i) == getSize sizes i) [0 .. n-1]
    assert UnbalNotBal $ not (couldBeBalanced sh arr sizes)
    traverse_ (validTree Unbal (down sh)) arr
  where
    n = length arr
validTree desc sh (RRB.Leaf arr) = do
    assert LeafShift0 $ sh == 0
    assert LeafNumElems $ case desc of
        Bal -> n == blockSize
        BalRightEdge -> n >= 1 && n <= blockSize
        Unbal -> n >= 1 && n <= blockSize
  where
    n = length arr

-- | Check whether an Unbalanced node could be Balanced.
couldBeBalanced :: Shift -> A.Array (Tree a) -> PrimArray Int -> Bool
couldBeBalanced sh arr sizes =
   all (\i -> getSize sizes i == 1 `shiftL` sh) [0 .. n-2] &&
   (case A.last arr of
       Balanced _ -> True
       Unbalanced arr' sizes' -> couldBeBalanced (down sh) arr' sizes'
       Leaf _ -> True)
  where
    n = length arr

getSize :: PrimArray Int -> Int -> Int
getSize sizes 0 = indexPrimArray sizes 0
getSize sizes i = indexPrimArray sizes i - indexPrimArray sizes (i-1)

countElems :: Tree a -> Int
countElems (RRB.Balanced arr) =
    foldl' (\acc tree -> acc + countElems tree) 0 arr
countElems (RRB.Unbalanced arr _) =
    foldl' (\acc tree -> acc + countElems tree) 0 arr
countElems (RRB.Leaf arr) = length arr
