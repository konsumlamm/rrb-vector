{-# LANGUAGE BangPatterns #-}

import Data.Functor ((<&>))

import Test.Tasty.Bench

import qualified Data.RRBVector as RRB

default (Int)

main :: IO ()
main = defaultMain $ [10, 100, 1000, 10000, 100000] <&> \n ->
    let !v = RRB.fromList [1..n]
        !idx = n `div` 2
    in bgroup (show n)
    [ bench "fromList" $ whnf RRB.fromList [1..n]
    , bench "><" $ whnf (\vec -> vec RRB.>< vec) v
    , bench "|>" $ whnf (RRB.|> 42) v
    , bench "<|" $ whnf (42 RRB.<|) v
    , bench "take" $ whnf (RRB.take idx) v
    , bench "drop" $ whnf (RRB.drop idx) v
    , bench "splitAt" $ whnf (RRB.splitAt idx) v
    , bench "insertAt" $ whnf (RRB.insertAt idx 42) v
    , bench "deleteAt" $ whnf (RRB.deleteAt idx) v
    , bench "index" $ nf (RRB.lookup idx) v
    , bench "adjust" $ whnf (RRB.adjust idx (+ 1)) v
    , bench "map" $ whnf (RRB.map (+ 1)) v
    , bench "foldl" $ nf (foldl (+) 0) v
    , bench "foldr" $ nf (foldr (+) 0) v
    , bench "findIndexL" $ nf (RRB.findIndexL (== idx)) v
    , bench "findIndexR" $ nf (RRB.findIndexR (== idx)) v
    , bench "findIndicesL" $ nf (RRB.findIndicesL (== idx)) v
    , bench "findIndicesR" $ nf (RRB.findIndicesR (== idx)) v
    ]
