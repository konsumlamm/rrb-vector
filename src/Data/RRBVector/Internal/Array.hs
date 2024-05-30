{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}

-- | This is an internal module.
--
-- It provides a thin wrapper over "Data.Primitive.SmallArray"
-- with \(O(1)\) slicing.
--
-- __Warning:__ No bound checks are performed!

module Data.RRBVector.Internal.Array
    ( Array, MutableArray
    , ifoldrStep, ifoldlStep, ifoldrStep', ifoldlStep'
    , foldrMap1, ifoldrMap1Step
    , empty, singleton, from2, wrap
    , replicate, replicateSnoc
    , index, head, last
    , update, adjust, adjust'
    , take, drop, splitAt
    , snoc, cons, (++)
    , map, map'
    , imapStep, imapStep'
    , unzipWith
    , traverse, traverse'
    , itraverseStep, itraverseStep'
    , new, read, write
    , freeze, thaw
    ) where

#if !(MIN_VERSION_base(4,18,0))
import Control.Applicative (liftA2)
#endif
import Control.DeepSeq (NFData(..))
import Control.Monad (when)
import Control.Monad.ST
import Data.Foldable (Foldable(..))
import Data.Primitive.SmallArray
import Prelude hiding (replicate, take, drop, splitAt, head, last, map, traverse, read, unzip, (++))

-- start length array
data Array a = Array !Int !Int !(SmallArray a)
data MutableArray s a = MutableArray !Int !Int !(SmallMutableArray s a)

instance Foldable Array where
    foldr f z (Array start len arr) =
        let end = start + len
            go i
                | i == end = z
                | (# x #) <- indexSmallArray## arr i = f x (go (i + 1))
        in go start

    foldl f z (Array start len arr) =
        let go i
                | i < start = z
                | (# x #) <- indexSmallArray## arr i = f (go (i - 1)) x
        in go (start + len - 1)

    foldr' f z (Array start len arr) =
        let go i !acc
                | i < start = acc
                | (# x #) <- indexSmallArray## arr i = go (i - 1) (f x acc)
        in go (start + len - 1) z

    foldl' f z (Array start len arr) =
        let end = start + len
            go i !acc
                | i == end = acc
                | (# x #) <- indexSmallArray## arr i = go (i + 1) (f acc x)
        in go start z

    null arr = length arr == 0

    length (Array _ len _) = len

instance (NFData a) => NFData (Array a) where
    rnf = foldl' (\_ x -> rnf x) ()

ifoldrStep :: Int -> (a -> Int) -> (Int -> a -> b -> b) -> b -> Array a -> b
ifoldrStep i0 step f z (Array start len arr) =
    let end = start + len
        go !i !j -- i is the index in arr, j is the index for f
            | i == end = z
            | (# x #) <- indexSmallArray## arr i = f j x (go (i + 1) (j + step x))
    in go start i0

ifoldlStep :: Int -> (a -> Int) -> (Int -> b -> a -> b) -> b -> Array a -> b
ifoldlStep i0 step f z (Array start len arr) =
    let go !i !j -- i is the index in arr, j is the index for f
            | i < start = z
            | (# x #) <- indexSmallArray## arr i = f j (go (i - 1) (j - step x)) x
    in go (start + len - 1) i0

ifoldrStep' :: Int -> (a -> Int) -> (Int -> a -> b -> b) -> b -> Array a -> b
ifoldrStep' i0 step f z (Array start len arr) =
    let go !i !j !acc -- i is the index in arr, j is the index for f
            | i < start = acc
            | (# x #) <- indexSmallArray## arr i = go (i - 1) (j - step x) (f j x acc)
    in go (start + len - 1) i0 z

ifoldlStep' :: Int -> (a -> Int) -> (Int -> b -> a -> b) -> b -> Array a -> b
ifoldlStep' i0 step f z (Array start len arr) =
    let end = start + len
        go !i !j !acc -- i is the index in arr, j is the index for f
            | i == end = acc
            | (# x #) <- indexSmallArray## arr i = go (i + 1) (j + step x) (f j acc x)
    in go start i0 z

-- helper function for implementing foldToMaybeTree
foldrMap1 :: (a -> b) -> (b -> b -> b) -> Array a -> b
foldrMap1 f g (Array start len arr) =
    let end = start + len
        go i
            | i == end - 1, (# x #) <- indexSmallArray## arr i = f x
            | (# x #) <- indexSmallArray## arr i = g (f x) (go (i + 1))
    in go start

-- helper function for implementing foldToMaybeWithIndexTree
ifoldrMap1Step :: Int -> (a -> Int) -> (Int -> a -> b) -> (b -> b -> b) -> Array a -> b
ifoldrMap1Step i0 step f g (Array start len arr) =
    let end = start + len
        go !i !j -- i is the index in arr, j is the index for f
            | i == end - 1, (# x #) <- indexSmallArray## arr i = f j x
            | (# x #) <- indexSmallArray## arr i = g (f j x) (go (i + 1) (j + step x))
    in go start i0

uninitialized :: a
uninitialized = errorWithoutStackTrace "uninitialized"

empty :: Array a
empty = Array 0 0 $ runSmallArray (newSmallArray 0 uninitialized)

singleton :: a -> Array a
singleton x = Array 0 1 $ runSmallArray (newSmallArray 1 x)

from2 :: a -> a -> Array a
from2 x y = Array 0 2 $ runSmallArray $ do
    sma <- newSmallArray 2 x
    writeSmallArray sma 1 y
    pure sma

wrap :: SmallArray a -> Array a
wrap arr = Array 0 (sizeofSmallArray arr) arr

replicate :: Int -> a -> Array a
replicate n x = Array 0 n $ runSmallArray (newSmallArray n x)

-- > replicateSnoc n x y = snoc (replicate n x) y
replicateSnoc :: Int -> a -> a -> Array a
replicateSnoc n x y = Array 0 len $ runSmallArray $ do
    sma <- newSmallArray len x
    writeSmallArray sma n y
    pure sma
  where
    len = n + 1

index :: Array a -> Int -> a
index (Array start _ arr) idx = indexSmallArray arr (start + idx)

update :: Array a -> Int -> a -> Array a
update (Array start len sa) idx x = Array 0 len $ runSmallArray $ do
    sma <- thawSmallArray sa start len
    writeSmallArray sma idx x
    pure sma

adjust :: Array a -> Int -> (a -> a) -> Array a
adjust (Array start len sa) idx f = Array 0 len $ runSmallArray $ do
    sma <- thawSmallArray sa start len
    x <- indexSmallArrayM sa (start + idx)
    writeSmallArray sma idx (f x)
    pure sma

adjust' :: Array a -> Int -> (a -> a) -> Array a
adjust' (Array start len sa) idx f = Array 0 len $ runSmallArray $ do
    sma <- thawSmallArray sa start len
    x <- indexSmallArrayM sa (start + idx)
    writeSmallArray sma idx $! f x
    pure sma

take :: Array a -> Int -> Array a
take (Array start _ arr) n = Array start n arr

drop :: Array a -> Int -> Array a
drop (Array start len arr) n = Array (start + n) (len - n) arr

splitAt :: Array a -> Int -> (Array a, Array a)
splitAt arr idx = (take arr idx, drop arr idx)

head :: Array a -> a
head arr = index arr 0

last :: Array a -> a
last arr = index arr (length arr - 1)

snoc :: Array a -> a -> Array a
snoc (Array start len arr) x = Array 0 len' $ runSmallArray $ do
    sma <- newSmallArray len' x
    copySmallArray sma 0 arr start len
    pure sma
  where
    !len' = len + 1

cons :: Array a -> a -> Array a
cons (Array start len arr) x = Array 0 len' $ runSmallArray $ do
    sma <- newSmallArray len' x
    copySmallArray sma 1 arr start len
    pure sma
  where
    !len' = len + 1

(++) :: Array a -> Array a -> Array a
Array start1 len1 arr1 ++ Array start2 len2 arr2 = Array 0 len' $ runSmallArray $ do
    sma <- newSmallArray len' uninitialized
    copySmallArray sma 0 arr1 start1 len1
    copySmallArray sma len1 arr2 start2 len2
    pure sma
  where
    !len' = len1 + len2

map :: (a -> b) -> Array a -> Array b
map f (Array start len arr) = Array 0 len $ runSmallArray $ do
    sma <- newSmallArray len uninitialized
    -- i is the index in arr, j is the index in sma
    let loop i j = when (j < len) $ do
            x <- indexSmallArrayM arr i
            writeSmallArray sma j (f x)
            loop (i + 1) (j + 1)
    loop start 0
    pure sma

map' :: (a -> b) -> Array a -> Array b
map' f (Array start len arr) = Array 0 len $ runSmallArray $ do
    sma <- newSmallArray len uninitialized
    -- i is the index in arr, j is the index in sma
    let loop i j = when (j < len) $ do
            x <- indexSmallArrayM arr i
            writeSmallArray sma j $! f x
            loop (i + 1) (j + 1)
    loop start 0
    pure sma

-- helper function for implementing imap
imapStep :: Int -> (a -> Int) -> (Int -> a -> b) -> Array a -> Array b
imapStep i0 step f (Array start len arr) = Array 0 len $ runSmallArray $ do
    sma <- newSmallArray len uninitialized
    -- i is the index in arr, j is the index in sma, k is the index for f
    let loop !i !j !k = when (j < len) $ do
            x <- indexSmallArrayM arr i
            writeSmallArray sma j (f k x)
            loop (i + 1) (j + 1) (k + step x)
    loop start 0 i0
    pure sma

-- helper function for implementing imap
imapStep' :: Int -> (a -> Int) -> (Int -> a -> b) -> Array a -> Array b
imapStep' i0 step f (Array start len arr) = Array 0 len $ runSmallArray $ do
    sma <- newSmallArray len uninitialized
    -- i is the index in arr, j is the index in sma, k is the index for f
    let loop !i !j !k = when (j < len) $ do
            x <- indexSmallArrayM arr i
            writeSmallArray sma j $! f k x
            loop (i + 1) (j + 1) (k + step x)
    loop start 0 i0
    pure sma

unzipWith :: (a -> (b, c)) -> Array a -> (Array b, Array c)
unzipWith f (Array start len arr) = runST $ do
    sma1 <- newSmallArray len uninitialized
    sma2 <- newSmallArray len uninitialized
    -- i is the index in arr, j is the index in sma1/sma2
    let loop i j = when (j < len) $ do
            val <- indexSmallArrayM arr i
            let !(x, y) = f val
            writeSmallArray sma1 j x
            writeSmallArray sma2 j y
            loop (i + 1) (j + 1)
    loop start 0
    arr1 <- unsafeFreezeSmallArray sma1
    arr2 <- unsafeFreezeSmallArray sma2
    pure (Array 0 len arr1, Array 0 len arr2)

newtype STA a = STA (forall s. SmallMutableArray s a -> ST s (SmallArray a))

runSTA :: Int -> STA a -> Array a
runSTA len (STA m) = Array 0 len (runST $ newSmallArray len uninitialized >>= m)

traverse :: (Applicative f) => (a -> f b) -> Array a -> f (Array b)
traverse f (Array start len arr) =
    -- i is the index in arr, j is the index in sma
    let go i j
            | j == len = pure $ STA unsafeFreezeSmallArray
            | (# x #) <- indexSmallArray## arr i = liftA2 (\y (STA m) -> STA $ \sma -> writeSmallArray sma j y *> m sma) (f x) (go (i + 1) (j + 1))
    in runSTA len <$> go start 0

traverse' :: (Applicative f) => (a -> f b) -> Array a -> f (Array b)
traverse' f (Array start len arr) =
    -- i is the index in arr, j is the index in sma
    let go i j
            | j == len = pure $ STA unsafeFreezeSmallArray
            | (# x #) <- indexSmallArray## arr i = liftA2 (\ !y (STA m) -> STA $ \sma -> writeSmallArray sma j y *> m sma) (f x) (go (i + 1) (j + 1))
    in runSTA len <$> go start 0

-- helper function for implementing itraverse
itraverseStep :: (Applicative f) => Int -> (a -> Int) -> (Int -> a -> f b) -> Array a -> f (Array b)
itraverseStep i0 step f (Array start len arr) =
    -- i is the index in arr, j is the index in sma, k is the index for f
    let go !i !j !k
            | j == len = pure $ STA unsafeFreezeSmallArray
            | (# x #) <- indexSmallArray## arr i = liftA2 (\y (STA m) -> STA $ \sma -> writeSmallArray sma j y *> m sma) (f k x) (go (i + 1) (j + 1) (k + step x))
    in runSTA len <$> go start 0 i0

-- helper function for implementing itraverse
itraverseStep' :: (Applicative f) => Int -> (a -> Int) -> (Int -> a -> f b) -> Array a -> f (Array b)
itraverseStep' i0 step f (Array start len arr) =
    -- i is the index in arr, j is the index in sma, k is the index for f
    let go !i !j !k
            | j == len = pure $ STA unsafeFreezeSmallArray
            | (# x #) <- indexSmallArray## arr i = liftA2 (\ !y (STA m) -> STA $ \sma -> writeSmallArray sma j y *> m sma) (f k x) (go (i + 1) (j + 1) (k + step x))
    in runSTA len <$> go start 0 i0

new :: Int -> ST s (MutableArray s a)
new len = MutableArray 0 len <$> newSmallArray len uninitialized

read :: MutableArray s a -> Int -> ST s a
read (MutableArray start _ arr) idx = readSmallArray arr (start + idx)

write :: MutableArray s a -> Int -> a -> ST s ()
write (MutableArray start _ arr) idx = writeSmallArray arr (start + idx)

freeze :: MutableArray s a -> Int -> Int -> ST s (Array a)
freeze (MutableArray start _ arr) idx len = Array 0 len <$> freezeSmallArray arr (start + idx) len

thaw :: Array a -> Int -> Int -> ST s (MutableArray s a)
thaw (Array start _ arr) idx len = MutableArray 0 len <$> thawSmallArray arr (start + idx) len
