module Util.Internal.IntRef
    ( IntRef
    , newIntRef
    , readIntRef
    , writeIntRef
    , modifyIntRef
    ) where

import Control.Monad.ST
import Data.Primitive.PrimArray

newtype IntRef s = IntRef (MutablePrimArray s Int)

newIntRef :: Int -> ST s (IntRef s)
newIntRef i = do
    arr <- newPrimArray 1
    setPrimArray arr 0 1 i
    pure (IntRef arr)
{-# INLINE newIntRef #-}

readIntRef :: IntRef s -> ST s Int
readIntRef (IntRef arr) = readPrimArray arr 0
{-# INLINE readIntRef #-}

writeIntRef :: IntRef s -> Int -> ST s ()
writeIntRef (IntRef arr) = writePrimArray arr 0
{-# INLINE writeIntRef #-}

modifyIntRef :: IntRef s -> (Int -> Int) -> ST s ()
modifyIntRef (IntRef arr) f = do
    i <- readPrimArray arr 0
    writePrimArray arr 0 (f i)
{-# INLINE modifyIntRef #-}
