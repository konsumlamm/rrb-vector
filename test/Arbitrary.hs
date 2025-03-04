{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

module Arbitrary () where

#if !(MIN_VERSION_base(4,18,0))
import Control.Applicative (liftA2)
#endif
import Data.Foldable (toList)

import Test.Tasty.QuickCheck

import qualified Data.RRBVector as V
import Data.RRBVector.Internal.Debug

shrinkOne :: (a -> [a]) -> V.Vector a -> [V.Vector a]
shrinkOne shr v = [V.update i x v | i <- [0 .. length v - 1], x <- shr (v V.! i)]

instance (Arbitrary a) => Arbitrary (V.Vector a) where
    arbitrary = arbitrary1
    shrink = shrink1

instance Arbitrary1 V.Vector where
    liftArbitrary gen = do
        xs <- listOf gen
        sizes <- infiniteListOf $ chooseInt (2, blockSize)
        pure $ fromListWithSizes xs sizes

    liftShrink shr v = case subtrees v of
        [] -> map V.fromList . liftShrink shr $ toList v
        ts -> ts ++ shrinkOne shr v

-- A custom 'Testable' instance to use 'showTree'.
instance {-# OVERLAPPING #-} (Arbitrary a, Show a, Testable prop) => Testable (V.Vector a -> prop) where
    property = propertyForAllShrinkShow arbitrary shrink (pure . showTree)

    propertyForAllShrinkShow gen shr shw f =
        propertyForAllShrinkShow
            (liftA2 (,) gen arbitrary)
            (liftShrink2 shr shrink)
            (\(x, y) -> shw x ++ [showTree y])
            (uncurry f)
