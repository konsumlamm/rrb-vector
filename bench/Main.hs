{-# LANGUAGE BangPatterns #-}

import Data.Functor ((<&>))

import Test.Tasty.Bench

import qualified Data.RRBVector as RRB

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
    , bench "index" $ nf (RRB.lookup idx) v
    , bench "adjust" $ whnf (RRB.adjust idx (+ 1)) v
    , bench "foldl" $ nf (foldl (+) 0) v
    , bench "foldr" $ nf (foldr (+) 0) v
    ]
