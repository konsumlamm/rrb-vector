import Data.Foldable (toList)
import Data.List (uncons)

import Test.Tasty
import Test.Tasty.QuickCheck

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
main = defaultMain . localOption (QuickCheckTests 10_000) . localOption (QuickCheckMaxSize 10_000) $ testGroup "rrb-vector"
    [ testProperty "satisfies `fromList . toList == id`" $ \v -> V.fromList (toList v) === v
    , testProperty "satisfies `toList . fromList == id`" $ \ls -> toList (V.fromList ls) === ls
    , testGroup "replicate"
        [ testProperty "satisifes `replicate n == fromList . replicate n`" $ \(Positive n) x -> V.replicate n x === V.fromList (replicate n x)
        , testProperty "returns the empty vector for non-positive n" $ \(NonPositive n) x -> V.replicate n x === V.empty
        ]
    , testGroup "lookup"
        [ testProperty "gets the element at the index" $ \v (NonNegative i) -> V.lookup i v === lookupList i (toList v)
        , testProperty "returns Nothing for negative indices" $ \v (Negative i) -> V.lookup i v === Nothing
        ]
    , testGroup "update"
        [ testProperty "updates the element at the index" $ \v (NonNegative i) x -> toList (V.update i x v) === updateList i x (toList v)
        , testProperty "returns the vector for negative indices" $ \v (Negative i) x -> V.update i x v === v
        ]
    , testGroup "adjust"
        [ testProperty "adjusts the element at the index" $ \v (NonNegative i) -> toList (V.adjust i (+ 1) v) === adjustList i (+ 1) (toList v)
        , testProperty "returns the vector for negative indices" $ \v (Negative i) -> V.adjust i (+ 1) v === v
        ]
    , testGroup "><"
        [ testProperty "concatenates two vectors" $ \v1 v2 -> toList (v1 V.>< v2) === toList v1 ++ toList v2
        , testProperty "works for the empty vector" $ \v -> (V.empty V.>< v === v) .&&. (v V.>< V.empty === v)
        ]
    , testGroup "|>"
        [ testProperty "appends an element" $ \v x -> toList (v V.|> x) === toList v ++ [x]
        , testProperty "works for the empty vector" $ \x -> V.empty V.|> x === V.singleton x
        ]
    , testGroup "<|"
        [ testProperty "prepends an element" $ \x v -> toList (x V.<| v) === x : toList v
        , testProperty "works for the empty vector" $ \x -> x V.<| V.empty === V.singleton x
        ]
    , testGroup "take"
        [ testProperty "takes n elements" $ \v (Positive n) -> toList (V.take n v) === take n (toList v)
        , testProperty "returns the empty vector for non-positive n" $ \v (NonPositive n) -> V.take n v === V.empty
        ]
    , testGroup "drop"
        [ testProperty "drops n elements" $ \v (Positive n) -> toList (V.drop n v) === drop n (toList v)
        , testProperty "does nothing for non-positive n" $ \v (NonPositive n) -> V.drop n v === v
        ]
    , testGroup "viewl"
        [ testProperty "works like uncons" $ \v -> fmap (\(x, xs) -> (x, toList xs)) (V.viewl v) === uncons (toList v)
        , testProperty "works for the empty vector" $ V.viewl V.empty === Nothing
        ]
    , testGroup "viewr"
        [ testProperty "works like unsnoc" $ \v -> fmap (\(xs, x) -> (toList xs, x)) (V.viewr v) === unsnoc (toList v)
        , testProperty "works for the empty vector" $ V.viewr V.empty === Nothing
        ]
    , testGroup "map"
        [ testProperty "maps over the vector" $ \v -> toList (V.map (+ 1) v) === map (+ 1) (toList v)
        ]
    ]