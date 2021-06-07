module Data.RRBVector.Internal.Buffer
    ( Buffer
    , new
    , push
    , get
    , size
    ) where

import Control.Monad.ST

import Data.RRBVector.Internal.IntRef
import qualified Data.RRBVector.Internal.Array as A

data Buffer s a = Buffer !(A.MutableArray s a) !(IntRef s)

new :: Int -> ST s (Buffer s a)
new capacity = do
    buffer <- A.new capacity
    offset <- newIntRef 0
    pure (Buffer buffer offset)

push :: Buffer s a -> a -> ST s ()
push (Buffer buffer offset) x = do
    idx <- readIntRef offset
    A.write buffer idx x
    modifyIntRef offset (+ 1)

get :: Buffer s a -> ST s (A.Array a)
get (Buffer buffer offset) = do
    len <- readIntRef offset
    result <- A.freeze buffer 0 len
    writeIntRef offset 0
    pure result

size :: Buffer s a -> ST s Int
size (Buffer _ offset) = readIntRef offset
{-# INLInE size #-}
