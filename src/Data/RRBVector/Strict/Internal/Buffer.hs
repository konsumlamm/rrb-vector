-- | This is an internal module.
--
-- A 'Buffer' is an array with a fixed capacity, used to build up 'Data.RRBVector.Internal.Array.Array's.
-- It is used in the implementation of 'Data.RRBVector.fromList' and 'Data.RRBVector.><'.
{-# LANGUAGE BangPatterns #-}

module Data.RRBVector.Strict.Internal.Buffer
    ( Buffer
    , new
    , push
    , get
    , size
    ) where

import Control.Monad.ST

import Data.RRBVector.Strict.Internal.IntRef
import qualified Data.RRBVector.Strict.Internal.Array as A

-- | A mutable array buffer with a fixed capacity.
data Buffer s a = Buffer !(A.MutableArray s a) !(IntRef s)

-- | \(O(n)\). Create a new empty buffer with the given capacity.
new :: Int -> ST s (Buffer s a)
new capacity = do
    buffer <- A.new capacity
    offset <- newIntRef 0
    pure (Buffer buffer offset)

-- | \(O(1)\). Push a new element onto the buffer.
-- The size of the buffer must not exceed the capacity, but this is not checked.
push :: Buffer s a -> a -> ST s ()
push (Buffer buffer offset) !x = do
    idx <- readIntRef offset
    A.write buffer idx x
    writeIntRef offset (idx + 1)

-- | \(O(n)\). Freeze the content of the buffer and return it.
-- This resets the buffer so that it is empty.
get :: Buffer s a -> ST s (A.Array a)
get (Buffer buffer offset) = do
    len <- readIntRef offset
    result <- A.freeze buffer 0 len
    writeIntRef offset 0
    pure result

-- | \(O(1)\). Return the current size of the buffer.
size :: Buffer s a -> ST s Int
size (Buffer _ offset) = readIntRef offset
{-# INLInE size #-}
