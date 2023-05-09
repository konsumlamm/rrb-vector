{-# LANGUAGE PatternSynonyms #-}

{- |
This module contains some debug utilities. It should only be used for debugging/testing purposes.
-}

module Data.RRBVector.Strict.Internal.Debug
    ( showTree
    , fromListUnbalanced
    , pattern Empty, pattern Root
    , Tree, Shift
    , pattern Balanced, pattern Unbalanced, pattern Leaf
    ) where

import Control.Monad.ST (runST)
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Primitive.PrimArray (PrimArray, primArrayToList)

import Data.RRBVector.Strict.Internal hiding (Empty, Root, Balanced, Unbalanced, Leaf)
import qualified Data.RRBVector.Strict.Internal as RRB
import Data.RRBVector.Strict.Internal.Array (Array)
import qualified Data.RRBVector.Strict.Internal.Buffer as Buffer

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
