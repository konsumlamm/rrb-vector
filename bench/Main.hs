import Data.Functor ((<&>))

import Gauge.Main

import qualified Data.RRBVector as RRB

main :: IO ()
main = defaultMain $ [10, 100, 1_000, 10_000, 100_000] <&> \n ->
    let v = RRB.fromList [1..n]
        idx = [n `div` 10, n `div` 2, n - n `div` 10]
    in bgroup (show n)
    [ bench "fromList" $ nf RRB.fromList [1..n]
    , bench "><" $ nf (\vec -> vec RRB.>< vec) v
    , bench "|>" $ nf (RRB.|> 42) v
    , bench "<|" $ nf (42 RRB.<|) v
    , bgroup "take" $ idx <&> \i -> bench (show i) $ nf (RRB.take i) v
    , bgroup "drop" $ idx <&> \i -> bench (show i) $ nf (RRB.drop i) v
    , bgroup "lookup" $ idx <&> \i -> bench (show i) $ nf (RRB.lookup i) v
    , bgroup "adjust" $ idx <&> \i -> bench (show i) $ nf (RRB.adjust i (+ 1)) v
    , bench "foldl" $ nf (foldl (+) 0) v
    , bench "foldr" $ nf (foldr (+) 0) v
    ]
