{-# LANGUAGE RankNTypes #-}

module Util.Internal.Array where

import Control.Monad.ST
import Data.Primitive.SmallArray
import Prelude hiding (take, drop, splitAt, head, last)

-- TODO: save start (and end?) index for O(1) slicing

type Array a = SmallArray a
type MutableArray s a = SmallMutableArray s a

uninitialized :: a
uninitialized = errorWithoutStackTrace "uninitialized"

empty :: Array a
empty = create $ newSmallArray 0 uninitialized

singleton :: a -> Array a
singleton x = create $ newSmallArray 1 x
{-# INLINE singleton #-}

from2 :: a -> a -> Array a
from2 x y = create $ do
    arr <- newSmallArray 2 x
    writeSmallArray arr 1 y
    pure arr
{-# INLINE from2 #-}

fromList :: [a] -> Array a
fromList = smallArrayFromList
{-# INLINE fromList #-}

index :: Array a -> Int -> a
index = indexSmallArray
{-# INLINE index #-}

adjust :: Array a -> Int -> (a -> a) -> Array a
adjust arr idx f = create $ do
    newArr <- thawSmallArray arr 0 (length arr)
    let x = indexSmallArray arr idx
    writeSmallArray newArr idx (f x)
    pure newArr
{-# INLINE adjust #-}

take :: Array a -> Int -> Array a
take arr = cloneSmallArray arr 0
{-# INLINE take #-}

drop :: Array a -> Int -> Array a
drop arr n = cloneSmallArray arr n (length arr - n)
{-# INLINE drop #-}

splitAt :: Array a -> Int -> (Array a, Array a)
splitAt arr idx = (take arr idx, drop arr idx)
{-# INLINE splitAt #-}

head :: Array a -> a
head arr = index arr 0
{-# INLINE head #-}

last :: Array a -> a
last arr = index arr (length arr - 1)
{-# INLINE last #-}

snoc :: Array a -> a -> Array a
snoc arr x = create $ do
    newArr <- newSmallArray (length arr + 1) x
    copySmallArray newArr 0 arr 0 (length arr)
    pure newArr
{-# INLINE snoc #-}

create :: (forall s. ST s (MutableArray s a)) -> Array a
create = runSmallArray
{-# INLINE create #-}

new :: Int -> ST s (MutableArray s a)
new len = newSmallArray len uninitialized
{-# INLINE new #-}

read :: MutableArray s a -> Int -> ST s a
read = readSmallArray
{-# INLINE read #-}

write :: MutableArray s a -> Int -> a -> ST s ()
write = writeSmallArray
{-# INLINE write #-}

freeze :: MutableArray s a -> Int -> Int -> ST s (Array a)
freeze = freezeSmallArray
{-# INLINE freeze #-}

thaw :: Array a -> Int -> Int -> ST s (MutableArray s a)
thaw = thawSmallArray
{-# INLINE thaw #-}
