{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

#define GHC_HEAP_VIEW defined(VERSION_ghc_heap_view)

module Strictness
    ( strictness
    ) where

#if GHC_HEAP_VIEW
import Control.DeepSeq (deepseq)
import GHC.AssertNF (isNF)
#endif
import qualified Data.RRBVector as V
import Test.Tasty
import Test.Tasty.QuickCheck

import Arbitrary ()

default (Int)

#if GHC_HEAP_VIEW
testNF :: a -> Property
testNF !x = ioProperty (isNF x)

tailVector :: V.Vector a -> Maybe (V.Vector a)
tailVector v = case V.viewl v of
    Nothing -> Nothing
    Just (_, xs) -> Just xs

initVector :: V.Vector a -> Maybe (V.Vector a)
initVector v = case V.viewr v of
    Nothing -> Nothing
    Just (xs, _) -> Just xs
#endif

strictness :: TestTree
strictness = testGroup "strictness"
#if GHC_HEAP_VIEW
    [ testGroup "nf"
        [ testProperty "empty" $ testNF V.empty
        , testProperty "singleton" $ testNF (V.singleton 42)
        , testProperty "fromList" $ \ls -> ls `deepseq` testNF (V.fromList ls)
        , testProperty "replicate" $ \n -> testNF (V.replicate n 42)
        , testProperty "update" $ \v (NonNegative i) -> v `deepseq` testNF (V.update i 42 v)
        , testProperty "adjust'" $ \v (NonNegative i) -> v `deepseq` testNF (V.adjust' i (+ 1) v)
        , testProperty "<|" $ \v -> v `deepseq` testNF (42 V.<| v)
        , testProperty "|>" $ \v -> v `deepseq` testNF (v V.|> 42)
        , testProperty "><" $ \v1 v2 -> v1 `deepseq` v2 `deepseq` testNF (v1 V.>< v2)
        , testProperty "take" $ \v n -> v `deepseq` testNF (V.take n v)
        , testProperty "drop" $ \v n -> v `deepseq` testNF (V.drop n v)
        , testProperty "splitAt" $ \v n -> v `deepseq` testNF (V.splitAt n v)
        , testProperty "insertAt" $ \v i -> v `deepseq` testNF (V.insertAt i 42 v)
        , testProperty "deleteAt" $ \v i -> v `deepseq` testNF (V.deleteAt i v)
        , testProperty "viewl (tail)" $ \v -> v `deepseq` testNF (tailVector v)
        , testProperty "viewr (init)" $ \v -> v `deepseq` testNF (initVector v)
        , testProperty "map'" $ \v -> v `deepseq` testNF (V.map' (+ 1) v)
        , testProperty "reverse" $ \v -> v `deepseq` testNF (V.reverse v)
        , testProperty "zip" $ \v1 v2 -> v1 `deepseq` v2 `deepseq` testNF (V.zip v1 v2)
        , testProperty "unzip" $ \v -> v `deepseq` testNF (V.unzip v)
        ]
    , testGroup "bottom"
#else
    [ testGroup "bottom"
#endif
        [ testProperty "singleton" $ V.singleton undefined `seq` ()
        , testProperty "fromList" $ \n -> V.fromList (replicate n undefined) `seq` ()
        , testProperty "replicate" $ \n -> V.replicate n undefined `seq` ()
        , testProperty "<|" $ \v -> undefined V.<| v `seq` ()
        , testProperty "|>" $ \v -> v V.|> undefined `seq` ()
        , testProperty "update" $ \v i -> V.update i undefined v `seq` ()
        , testProperty "adjust" $ \v i -> V.adjust i (const undefined) v `seq` ()
        , testProperty "insertAt" $ \v i -> V.insertAt i undefined v `seq` ()
        , testProperty "map" $ \v -> V.map (const undefined) v `seq` ()
        , testProperty "zipWith" $ \v1 v2 -> V.zipWith (\_ _ -> undefined) v1 v2 `seq` ()
        ]
    ]
