module Arbitrary where

import Test.Tasty.QuickCheck

import qualified Data.RRBVector as V
import Data.RRBVector.Internal.Debug

-- TODO: improve instance
instance (Arbitrary a) => Arbitrary (V.Vector a) where
    arbitrary = oneof [V.fromList <$> arbitrary, fromListUnbalanced <$> arbitrary]

