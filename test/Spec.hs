import Data.Foldable (toList)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.RRBVector as V

default (Int)

instance (Arbitrary a) => Arbitrary (V.Vector a) where
    -- TODO: also generate unbalanced trees
    arbitrary = V.fromList <$> arbitrary

lookupList :: Int -> [a] -> Maybe a
lookupList i ls
    | i < length ls = Just (ls !! i)
    | otherwise = Nothing

adjustList :: Int -> (a -> a) -> [a] -> [a]
adjustList i f ls
    | i < length ls = let (left, x : right) = splitAt i ls in left ++ (f x : right)
    | otherwise = ls

main :: IO ()
main = hspec $ modifyMaxSuccess (const 1000) $ do
    prop "satisfies `fromList . toList == id`" $ \v -> V.fromList (toList v) === v
    prop "satisfies `toList . fromList == id`" $ \ls -> toList (V.fromList ls) === ls

    describe "lookup" $ do
        prop "gets the element at the index" $ \v (NonNegative i) -> V.lookup i v === lookupList i (toList v)
        prop "returns Nothing for negative indices" $ \v (Negative i) -> V.lookup i v === Nothing

    describe "adjust" $ do
        prop "gets the element at the index" $ \v (NonNegative i) -> toList (V.adjust i (+ 1) v) === adjustList i (+ 1) (toList v)
        prop "returns the vector for negative indices" $ \v (Negative i) -> V.adjust i (+ 1) v === v

    describe "><" $ do
        prop "concatenates two vectors" $ \v1 v2 -> toList (v1 V.>< v2) === toList v1 ++ toList v2
        prop "works for the empty vector" $ \v -> (V.empty V.>< v `shouldBe` v) .&&. (v V.>< V.empty `shouldBe` v)

    describe "take" $ do
        prop "takes n elements" $ \v (Positive n) -> toList (V.take n v) === take n (toList v)
        prop "works for non-positive n" $ \v (NonPositive n) -> V.take n v === V.empty

    describe "drop" $ do
        prop "drops n elements" $ \v (Positive n) -> toList (V.drop n v) === drop n (toList v)
        prop "works for non-positive n" $ \v (NonPositive n) -> V.drop n v === v
