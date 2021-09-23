{-# LANGUAGE FlexibleInstances #-}

module Arbitrary where

import Control.Applicative (liftA2)

import Test.Tasty.QuickCheck

import qualified Data.RRBVector as V
import Data.RRBVector.Internal.Debug

-- TODO: improve instance
instance (Arbitrary a) => Arbitrary (V.Vector a) where
    arbitrary = elements [V.fromList, fromListUnbalanced, foldr (V.<|) V.empty] >>= (<$> arbitrary)

-- A custom 'Testable' instance to use 'showTree'.
instance {-# OVERLAPPING #-} (Arbitrary a, Show a, Testable prop) => Testable (V.Vector a -> prop) where
    property f = propertyForAllShrinkShow arbitrary shrink (pure . showTree) f
    propertyForAllShrinkShow gen shr shw f =
        propertyForAllShrinkShow
            (liftA2 (,) gen arbitrary)
            (liftShrink2 shr shrink)
            (\(x, y) -> shw x ++ [showTree y])
            (uncurry f)
