{-# LANGUAGE CPP #-}

module Strictness
    ( strictness
    ) where

#ifdef VERSION_nothunks
import Control.DeepSeq (deepseq)
import Data.Foldable (foldr', foldl', toList)
import Data.Maybe (isNothing)
import Data.RRBVector.Strict.Internal.Debug
import NoThunks.Class
#endif

import qualified Data.RRBVector.Strict as V
import Test.Tasty
import Test.Tasty.QuickCheck

import Arbitrary ()

default (Int)

#ifdef VERSION_nothunks
instance (NoThunks a) => NoThunks (V.Vector a) where
    showTypeOf _ = "Vector"

    wNoThunks _ Empty = pure Nothing
    wNoThunks ctx (Root _ _ tree) = wNoThunks ctx tree

instance (NoThunks a) => NoThunks (Tree a) where
    showTypeOf _ = "Tree"

    wNoThunks ctx (Balanced arr) = noThunksInValues ctx (toList arr)
    wNoThunks ctx (Unbalanced arr _) = noThunksInValues ctx (toList arr)
    wNoThunks ctx (Leaf arr) = noThunksInValues ctx (toList arr)

testNF :: (NoThunks a) => a -> Property
testNF x = x `seq` ioProperty (isNothing <$> wNoThunks [] x)

tailVector :: V.Vector a -> Maybe (V.Vector a)
tailVector v = case V.viewl v of
    Nothing -> Nothing
    Just (_, xs) -> Just xs

initVector :: V.Vector a -> Maybe (V.Vector a)
initVector v = case V.viewr v of
    Nothing -> Nothing
    Just (xs, _) -> Just xs
#endif

inc :: Int -> Int
inc x = x + 1
{-# NOINLINE inc#-}

strictness :: TestTree
strictness = testGroup "strictness"
   [
#ifdef VERSION_nothunks
     localOption (QuickCheckTests 500) $ testGroup "nf"
        [ testProperty "empty" $ testNF (V.empty :: V.Vector Int)
        , testProperty "singleton" $ testNF (V.singleton $ inc 42)
        , testProperty "fromList" $ \ls -> let ls' = map inc ls in testNF (V.fromList ls')
        , testProperty "replicate" $ \n -> testNF (V.replicate n $ inc 43)
        , testProperty "update" $ \v (NonNegative i) -> v `deepseq` testNF (V.update i (inc 44) v)
        , testProperty "adjust'" $ \v (NonNegative i) -> v `deepseq` testNF (V.adjust' i inc v)
        , testProperty "<|" $ \v -> v `deepseq` testNF (inc 45 V.<| v)
        , testProperty "|>" $ \v -> v `deepseq` testNF (v V.|> inc 46)
        , testProperty "><" $ \v1 v2 -> v1 `deepseq` v2 `deepseq` testNF (v1 V.>< v2)
        , testProperty "take" $ \v n -> v `deepseq` testNF (V.take n v)
        , testProperty "drop" $ \v n -> v `deepseq` testNF (V.drop n v)
        , testProperty "splitAt" $ \v n -> v `deepseq` testNF (V.splitAt n v)
        , testProperty "insertAt" $ \v i -> v `deepseq` testNF (V.insertAt i (inc 47) v)
        , testProperty "deleteAt" $ \v i -> v `deepseq` testNF (V.deleteAt i v)
        , testProperty "viewl (tail)" $ \v -> v `deepseq` testNF (tailVector v)
        , testProperty "viewr (init)" $ \v -> v `deepseq` testNF (initVector v)
        , testProperty "map'" $ \v -> v `deepseq` testNF (V.map' inc v)
        , testProperty "reverse" $ \v -> v `deepseq` testNF (V.reverse v)
        , testProperty "zip" $ \v1 v2 -> v1 `deepseq` v2 `deepseq` testNF (V.zip v1 v2)
        , testProperty "unzip" $ \v -> v `deepseq` testNF (V.unzip v)
        , testProperty "foldr'" $ \v -> (v :: V.Vector Int) `deepseq` testNF (foldr' (:) [] v)
        , testProperty "foldl'" $ \v -> (v :: V.Vector Int) `deepseq` testNF (foldl' (flip (:)) [] v)
        , testProperty "ifoldr'" $ \v -> (v :: V.Vector Int) `deepseq` testNF (V.ifoldr' (const (:)) [] v)
        , testProperty "ifoldl'" $ \v -> (v :: V.Vector Int) `deepseq` testNF (V.ifoldl' (const (flip (:))) [] v)
        ]
#endif
    ]
