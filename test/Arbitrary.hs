{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

module Arbitrary where

#if !(MIN_VERSION_base(4,18,0))
import Control.Applicative (liftA2)
#endif
import Data.Foldable (toList)

import Test.Tasty.QuickCheck

import qualified Data.RRBVector as V
import Data.RRBVector.Internal.Debug

builders :: [[a] -> V.Vector a]
builders = [V.fromList, fromListUnbalanced]

instance (Arbitrary a) => Arbitrary (V.Vector a) where
    arbitrary = arbitrary1
    shrink = shrink1

-- TODO: improve instance
instance Arbitrary1 V.Vector where
    liftArbitrary gen = do
        build <- elements builders
        fmap build (liftArbitrary gen)

    liftShrink shr = concatMap (sequence builders) . liftShrink shr . toList

-- A custom 'Testable' instance to use 'showTree'.
instance {-# OVERLAPPING #-} (Arbitrary a, Show a, Testable prop) => Testable (V.Vector a -> prop) where
    property = propertyForAllShrinkShow arbitrary shrink (pure . showTree)

    propertyForAllShrinkShow gen shr shw f =
        propertyForAllShrinkShow
            (liftA2 (,) gen arbitrary)
            (liftShrink2 shr shrink)
            (\(x, y) -> shw x ++ [showTree y])
            (uncurry f)
