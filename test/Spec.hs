import Data.Foldable (toList)
import Data.List (uncons)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.RRBVector as V
import Data.RRBVector.Internal.Debug (fromListUnbalanced)

default (Int)

instance (Arbitrary a) => Arbitrary (V.Vector a) where
    arbitrary = oneof [V.fromList <$> arbitrary, fromListUnbalanced <$> arbitrary]

lookupList :: Int -> [a] -> Maybe a
lookupList i ls
    | i < length ls = Just (ls !! i)
    | otherwise = Nothing

updateList :: Int -> a -> [a] -> [a]
updateList i x ls
    | i < length ls = let (left, _ : right) = splitAt i ls in left ++ (x : right)
    | otherwise = ls

adjustList :: Int -> (a -> a) -> [a] -> [a]
adjustList i f ls
    | i < length ls = let (left, x : right) = splitAt i ls in left ++ (f x : right)
    | otherwise = ls

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc ls = Just (init ls, last ls)

main :: IO ()
main = hspec . modifyMaxSuccess maxN . modifyMaxSize maxN $ do
    prop "satisfies `fromList . toList == id`" $ \v -> V.fromList (toList v) === v
    prop "satisfies `toList . fromList == id`" $ \ls -> toList (V.fromList ls) === ls

    describe "replicate" $ do
        prop "satisifes `replicate n == fromList . replicate n`" $ \(Positive n) x -> V.replicate n x === V.fromList (replicate n x)
        prop "returns the empty vector for non-positive n" $ \(NonPositive n) x -> V.replicate n x === V.empty

    describe "lookup" $ do
        prop "gets the element at the index" $ \v (NonNegative i) -> V.lookup i v === lookupList i (toList v)
        prop "returns Nothing for negative indices" $ \v (Negative i) -> V.lookup i v === Nothing

    describe "update" $ do
        prop "updates the element at the index" $ \v (NonNegative i) x -> toList (V.update i x v) === updateList i x (toList v)
        prop "returns the vector for negative indices" $ \v (Negative i) x -> V.update i x v === v

    describe "adjust" $ do
        prop "adjusts the element at the index" $ \v (NonNegative i) -> toList (V.adjust i (+ 1) v) === adjustList i (+ 1) (toList v)
        prop "returns the vector for negative indices" $ \v (Negative i) -> V.adjust i (+ 1) v === v

    describe "><" $ do
        prop "concatenates two vectors" $ \v1 v2 -> toList (v1 V.>< v2) === toList v1 ++ toList v2
        prop "works for the empty vector" $ \v -> (V.empty V.>< v `shouldBe` v) .&&. (v V.>< V.empty `shouldBe` v)

    describe "|>" $ do
        prop "appends an element" $ \v x -> toList (v V.|> x) === toList v ++ [x]
        prop "works for the empty vector" $ \x -> V.empty V.|> x `shouldBe` V.singleton x

    describe "<|" $ do
        prop "prepends an element" $ \x v -> toList (x V.<| v) === x : toList v
        prop "works for the empty vector" $ \x -> x V.<| V.empty `shouldBe` V.singleton x

    describe "take" $ do
        prop "takes n elements" $ \v (Positive n) -> toList (V.take n v) === take n (toList v)
        prop "returns the empty vector for non-positive n" $ \v (NonPositive n) -> V.take n v === V.empty

    describe "drop" $ do
        prop "drops n elements" $ \v (Positive n) -> toList (V.drop n v) === drop n (toList v)
        prop "does nothing for non-positive n" $ \v (NonPositive n) -> V.drop n v === v

    describe "viewl" $ do
        prop "works like uncons" $ \v -> fmap (\(x, xs) -> (x, toList xs)) (V.viewl v) === uncons (toList v)
        prop "works for the empty vector" $ V.viewl V.empty `shouldBe` Nothing

    describe "viewr" $ do
        prop "works like unsnoc" $ \v -> fmap (\(xs, x) -> (toList xs, x)) (V.viewr v) === unsnoc (toList v)
        prop "works for the empty vector" $ V.viewr V.empty `shouldBe` Nothing

    describe "map" $ do
        prop "maps over the vector" $ \v -> toList (V.map (+ 1) v) === map (+ 1) (toList v)
  where
    maxN = const 10_000
