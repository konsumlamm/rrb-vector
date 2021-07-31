module Data.RRBVector.Internal.IntRef
    ( IntRef
    , newIntRef
    , readIntRef
    , writeIntRef
    ) where

import Control.Monad.ST
import Data.Primitive.PrimArray

newtype IntRef s = IntRef (MutablePrimArray s Int)

newIntRef :: Int -> ST s (IntRef s)
newIntRef i = do
    arr <- newPrimArray 1
    writePrimArray arr 0 i
    pure (IntRef arr)
{-# INLINE newIntRef #-}

readIntRef :: IntRef s -> ST s Int
readIntRef (IntRef arr) = readPrimArray arr 0
{-# INLINE readIntRef #-}

writeIntRef :: IntRef s -> Int -> ST s ()
writeIntRef (IntRef arr) = writePrimArray arr 0
{-# INLINE writeIntRef #-}
