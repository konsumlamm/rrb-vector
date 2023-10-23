{-# LANGUAGE CPP #-}

module Properties
    ( properties
    ) where

#if !(MIN_VERSION_base(4,18,0))
import Control.Applicative (liftA2)
#endif
import Data.Foldable (Foldable(..))
import Data.List (uncons)
import Data.Proxy (Proxy(..))
import Prelude hiding ((==)) -- use @===@ instead

import qualified Data.RRBVector.Strict as V
import Test.QuickCheck.Classes.Base
import Test.Tasty
import Test.Tasty.QuickCheck

import Arbitrary ()

default (Int)

type V = V.Vector

-- List equivalents of @Data.RRBVector@ functions

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

insertAtList :: Int -> a -> [a] -> [a]
insertAtList i x ls = let (left, right) = splitAt i ls in left ++ (x : right)

deleteAtList :: Int -> [a] -> [a]
deleteAtList i ls
    | i < length ls = let (left, _ : right) = splitAt i ls in left ++ right
    | otherwise = ls

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc ls = Just (init ls, last ls)


testLaws :: Laws -> TestTree
testLaws (Laws name pairs) = testGroup name (map (uncurry testProperty) pairs)

proxyVInt :: Proxy (V Int)
proxyVInt = Proxy

proxyV :: Proxy V
proxyV = Proxy

properties :: TestTree
properties = testGroup "properties"
    [ testGroup "fromList"
        [ testProperty "satisfies `fromList . toList = id`" $ \v -> V.fromList (toList v) === v
        , testProperty "satisfies `toList . fromList = id`" $ \ls -> toList (V.fromList ls) === ls
        , testProperty "satisfies `fromList [] = empty`" $ V.fromList [] === V.empty
        , testProperty "satisfies `fromList [x] = singleton x`" $ \x -> V.fromList [x] === V.singleton x
        ]
    , testGroup "replicate"
        [ testProperty "satisifes `replicate n == fromList . replicate n`" $ \(Positive n) x -> V.replicate n x === V.fromList (replicate n x)
        , testProperty "returns the empty vector for non-positive n" $ \(NonPositive n) x -> V.replicate n x === V.empty
        ]
    , testGroup "<|"
        [ testProperty "prepends an element" $ \x v -> toList (x V.<| v) === x : toList v
        , testProperty "works for the empty vector" $ \x -> x V.<| V.empty === V.singleton x
        ]
    , testGroup "|>"
        [ testProperty "appends an element" $ \v x -> toList (v V.|> x) === toList v ++ [x]
        , testProperty "works for the empty vector" $ \x -> V.empty V.|> x === V.singleton x
        ]
    , testGroup "><"
        [ testProperty "concatenates two vectors" $ \v1 v2 -> toList (v1 V.>< v2) === toList v1 ++ toList v2
        , testProperty "works for the empty vector" $ \v -> (V.empty V.>< v === v) .&&. (v V.>< V.empty === v)
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
        [ testProperty "adjusts the element at the index" $ \v (NonNegative i) (Fn f) -> toList (V.adjust i f v) === adjustList i f (toList v)
        , testProperty "returns the vector for negative indices" $ \v (Negative i) (Fn f) -> V.adjust i f v === v
        ]
    , testGroup "adjust'"
        [ testProperty "adjusts the element at the index" $ \v (NonNegative i) (Fn f) -> toList (V.adjust' i f v) === adjustList i f (toList v)
        , testProperty "returns the vector for negative indices" $ \v (Negative i) (Fn f) -> V.adjust' i f v === v
        ]
    , testGroup "viewl"
        [ testProperty "works like uncons" $ \v -> fmap (\(x, xs) -> (x, toList xs)) (V.viewl v) === uncons (toList v)
        , testProperty "works for the empty vector" $ V.viewl V.empty === Nothing
        ]
    , testGroup "viewr"
        [ testProperty "works like unsnoc" $ \v -> fmap (\(xs, x) -> (toList xs, x)) (V.viewr v) === unsnoc (toList v)
        , testProperty "works for the empty vector" $ V.viewr V.empty === Nothing
        ]
    , testGroup "take"
        [ testProperty "takes n elements" $ \v (Positive n) -> toList (V.take n v) === take n (toList v)
        , testProperty "returns the empty vector for non-positive n" $ \v (NonPositive n) -> V.take n v === V.empty
        ]
    , testGroup "drop"
        [ testProperty "drops n elements" $ \v (Positive n) -> toList (V.drop n v) === drop n (toList v)
        , testProperty "returns the vector for non-positive n" $ \v (NonPositive n) -> V.drop n v === v
        ]
    , testGroup "splitAt"
        [ testProperty "splits the vector" $ \v n -> let (v1, v2) = V.splitAt n v in (toList v1, toList v2) === splitAt n (toList v)
        ]
    , testGroup "insertAt"
        [ testProperty "inserts an element" $ \v i x -> toList (V.insertAt i x v) === insertAtList i x (toList v)
        , testProperty "prepends for negative indices" $ \v (Negative i) x -> V.insertAt i x v === x V.<| v
        , testProperty "appends for too large indices" $ \v x -> forAll (arbitrary `suchThat` (> length v)) $ \i -> V.insertAt i x v === v V.|> x
        , testProperty "satisfies `insertAt 0 x v = x <| v`" $ \v x -> V.insertAt 0 x v === x V.<| v
        , testProperty "satisfies `insertAt (length v) x v = v |> x`" $ \v x -> V.insertAt (length v) x v === v V.|> x
        ]
    , testGroup "deleteAt"
        [ testProperty "deletes an element" $ \v (NonNegative i) -> toList (V.deleteAt i v) === deleteAtList i (toList v)
        , testProperty "returns the vector for negative indices" $ \v (Negative i) -> V.deleteAt i v === v
        , testProperty "returns the vector for too large indices" $ \v -> forAll (arbitrary `suchThat` (>= length v)) $ \i -> V.deleteAt i v === v
        , testProperty "satisfies `deleteAt 0 v = drop 1 v`" $ \v -> V.deleteAt 0 v === V.drop 1 v
        , testProperty "satisfies `deleteAt (length v - 1) v = take (length v - 1) v`" $ \v -> V.deleteAt (length v - 1) v === V.take (length v - 1) v
        ]
    , testGroup "reverse"
        [ testProperty "reverses the vector" $ \v -> toList (V.reverse v) === reverse (toList v)
        ]
    , testGroup "zip"
        [ testProperty "zips two vectors" $ \v1 v2 -> toList (V.zip v1 v2) === zip (toList v1) (toList v2)
        ]
    , testGroup "zipWith"
        [ testProperty "zips two vectors with a function" $ \v1 v2 -> toList (V.zipWith (+) v1 v2) === zipWith (+) (toList v1) (toList v2)
        , testProperty "satisfies `zipWith (,) v1 v2 = zip v1 v2`" $ \v1 v2 -> V.zipWith (,) v1 v2 === V.zip v1 v2
        ]
    , testGroup "unzip"
        [ testProperty "unzips the vector" $ \v -> (\(xs, ys) -> (toList xs, toList ys)) (V.unzip v) === unzip (toList v)
        ]
    , instances
    , laws
    , issues
    ]

instances :: TestTree
instances = testGroup "instances"
    [ testGroup "Foldable"
        [ testProperty "foldr" $ \(v :: V Int) -> foldr (:) [] v === foldr (:) [] (toList v)
        , testProperty "foldl" $ \(v :: V Int) -> foldl (flip (:)) [] v === foldl (flip (:)) [] (toList v)
        , testProperty "foldr'" $ \(v :: V Int) -> foldr' (:) [] v === foldr' (:) [] (toList v)
        , testProperty "foldl'" $ \(v :: V Int) -> foldl' (flip (:)) [] v === foldl' (flip (:)) [] (toList v)
        ]
    , testGroup "FoldableWithIndex"
        [ testProperty "ifoldr" $ \(v :: V Int) -> V.ifoldr (\i x acc -> (i, x) : acc) [] v === V.ifoldr (\i x acc -> (i, x) : acc) [] (toList v)
        , testProperty "ifoldl" $ \(v :: V Int) -> V.ifoldl (\i acc x -> (i, x) : acc) [] v === V.ifoldl (\i acc x -> (i, x) : acc)  [] (toList v)
        , testProperty "ifoldr'" $ \(v :: V Int) -> V.ifoldr' (\i x acc -> (i, x) : acc) [] v === V.ifoldr' (\i x acc -> (i, x) : acc) [] (toList v)
        , testProperty "ifoldl'" $ \(v :: V Int) -> V.ifoldl' (\i acc x -> (i, x) : acc) [] v === V.ifoldl' (\i acc x -> (i, x) : acc)  [] (toList v)
        , testProperty "satisfies `ifoldr (const f) x v = foldr f x v`" $
            \(v :: V Int) -> V.ifoldr (const (:)) [] v === foldr (:) [] v
        , testProperty "satisfies `ifoldl (const f) x v = foldl f x v`" $
            \(v :: V Int) -> V.ifoldl (const (flip (:))) [] v === foldl (flip (:)) [] v
        ]
    , testGroup "Functor"
        [ testProperty "fmap" $ \v -> toList (V.map (+ 1) v) === map (+ 1) (toList v)
        ]
    , testGroup "FunctorWithIndex"
        [ testProperty "imap" $ \(v :: V Int) -> toList (V.imap (,) v) === V.imap (,) (toList v)
        , testProperty "satisfies `imap (const f) v = map f v`" $ \v -> V.imap (const (+ 1)) v === V.map (+ 1) v
        ]
    , testGroup "Traversable"
        [ testProperty "traverse" $
            \(v :: V Int) -> fmap toList (traverse (Just . (+ 1)) v) === traverse (Just . (+ 1)) (toList v)
        ]
    , testGroup "TraversableWithIndex"
        [ testProperty "itraverse" $
            \(v :: V Int) -> fmap toList (V.itraverse (\i x -> Just (i + x)) v) === V.itraverse (\i x -> Just (i + x)) (toList v)
        , testProperty "satisfies `itraverse (const f) v = traverse f v`" $
            \(v :: V Int) -> V.itraverse (const (Just . (+ 1))) v === traverse (Just . (+ 1)) v
        , testProperty "satisfies `imapDefault f v = imap f v`" $
            \(v :: V Int) -> V.imapDefault (,) v === V.imap (,) v
        ]
    , localOption (QuickCheckMaxSize 100) $ testGroup "Applicative"
        [ testProperty "liftA2" $
            \(v1 :: V Int) (v2 :: V Int) -> toList (liftA2 (,) v1 v2) === liftA2 (,) (toList v1) (toList v2)
        , testProperty "<*>" $
            \(v1 :: V Int) (v2 :: V Int) -> toList ((,) <$> v1 <*> v2) === ((,) <$> toList v1 <*> toList v2)
        , testProperty "*>" $
            \(v1 :: V Int) (v2 :: V Int) -> toList (v1 *> v2) === (toList v1 *> toList v2)
        , testProperty "<*" $
            \(v1 :: V Int) (v2 :: V Int) -> toList (v1 <* v2) === (toList v1 <* toList v2)
        ]
    ]

laws :: TestTree
laws = testGroup "typeclass laws"
    [ testLaws $ eqLaws proxyVInt
    , testLaws $ ordLaws proxyVInt
    , testLaws $ isListLaws proxyVInt
    , testLaws $ monoidLaws proxyVInt
    , localOption (QuickCheckTests 500) . localOption (QuickCheckMaxSize 1000) . testLaws $ semigroupLaws proxyVInt
    , localOption (QuickCheckMaxSize 500) . testLaws $ showLaws proxyVInt
    , localOption (QuickCheckMaxSize 1000) . testLaws $ showReadLaws proxyVInt
    , testLaws $ alternativeLaws proxyV
    , localOption (QuickCheckTests 100) . localOption (QuickCheckMaxSize 100) . testLaws $ applicativeLaws proxyV
    , localOption (QuickCheckTests 100) . testLaws $ foldableLaws proxyV
    , testLaws $ functorLaws proxyV
    , localOption (QuickCheckTests 100) . localOption (QuickCheckMaxSize 100) . testLaws $ monadLaws proxyV
    , testLaws $ monadPlusLaws proxyV
    , localOption (QuickCheckTests 500) . localOption (QuickCheckMaxSize 5000) . testLaws $ monadZipLaws proxyV
    , localOption (QuickCheckMaxSize 100) . testLaws $ traversableLaws proxyV
    ]

-- old issues, to avoid regressions
issues :: TestTree
issues = testGroup "issues"
    -- https://github.com/konsumlamm/rrb-vector/issues/10
    [ testProperty "#10" $ \x v -> case V.viewl v of
            Nothing -> property True
            Just (_, v') -> x V.<| v' === V.update 0 x v
    ]
