-- | This is an internal module.
--
-- It works like "Data.STRef", but is specialized to 'Int' and more efficient.

module Data.RRBVector.Strict.Internal.IntRef
    ( IntRef
    , newIntRef
    , readIntRef
    , writeIntRef
    ) where

import Control.Monad.ST
import Data.Primitive.PrimArray

-- | A mutable 'Int' reference.
newtype IntRef s = IntRef (MutablePrimArray s Int)

-- | \(O(1)\). Create a new 'IntRef' that is initialized with @0@.
newIntRef :: Int -> ST s (IntRef s)
newIntRef i = do
    arr <- newPrimArray 1
    writePrimArray arr 0 i
    pure (IntRef arr)
{-# INLINE newIntRef #-}

-- | \(O(1)\). Read the content of the 'IntRef'.
readIntRef :: IntRef s -> ST s Int
readIntRef (IntRef arr) = readPrimArray arr 0
{-# INLINE readIntRef #-}

-- | \(O(1)\). Write a value to the 'IntRef'.
writeIntRef :: IntRef s -> Int -> ST s ()
writeIntRef (IntRef arr) = writePrimArray arr 0
{-# INLINE writeIntRef #-}
