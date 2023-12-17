-- | This is an internal module.
--
-- A 'Buffer' is an array with a fixed capacity, used to build up 'Data.RRBVector.Internal.Array.Array's.
-- It is used in the implementation of 'Data.RRBVector.fromList' and 'Data.RRBVector.><'.

module Data.RRBVector.Internal.Buffer
    ( Buffer
    , new
    , push
    , get
    , size
    ) where

import Control.Monad.ST

import Data.Primitive.SmallArray
import Data.RRBVector.Internal.IntRef
import qualified Data.RRBVector.Internal.Array as A

-- | A mutable array buffer with a fixed capacity.
data Buffer s a = Buffer !(SmallMutableArray s a) !(IntRef s)

-- | \(O(n)\). Create a new empty buffer with the given capacity.
new :: Int -> ST s (Buffer s a)
new capacity = do
    buffer <- newSmallArray capacity uninitialized
    offset <- newIntRef 0
    pure (Buffer buffer offset)

uninitialized :: a
uninitialized = errorWithoutStackTrace "uninitialized"

-- | \(O(1)\). Push a new element onto the buffer.
-- The size of the buffer must not exceed the capacity, but this is not checked.
push :: Buffer s a -> a -> ST s ()
push (Buffer buffer offset) x = do
    idx <- readIntRef offset
    writeSmallArray buffer idx x
    writeIntRef offset (idx + 1)

-- | \(O(n)\). Freeze the content of the buffer and return it.
-- This resets the buffer so that it is empty.
get :: Buffer s a -> ST s (A.Array a)
get (Buffer buffer offset) = do
    len <- readIntRef offset
    result <- freezeSmallArray buffer 0 len
    writeIntRef offset 0
    pure (A.wrap result)

-- | \(O(1)\). Return the current size of the buffer.
size :: Buffer s a -> ST s Int
size (Buffer _ offset) = readIntRef offset
{-# INLInE size #-}
