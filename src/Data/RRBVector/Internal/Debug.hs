{- |
This module contains some debug utilities. It should only be used for debugging/testing purposes.
-}

module Data.RRBVector.Internal.Debug
    ( showTree
    , fromListUnbalanced
    ) where

import Control.Monad.ST (runST)
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Primitive.PrimArray (primArrayToList)

import Data.RRBVector.Internal
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
fromListUnbalanced [] = Empty
fromListUnbalanced [x] = singleton x
fromListUnbalanced ls = case nodes Leaf ls of
    [tree] -> Root (treeSize 0 tree) 0 tree -- tree is a single leaf
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
        [tree] -> Root (treeSize sh tree) sh tree
        trees' -> iterateNodes (up sh) trees'
