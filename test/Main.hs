{-# LANGUAGE BangPatterns #-}

import Test.Tasty
import Test.Tasty.QuickCheck

import Properties (properties)
import Strictness (strictness)

main :: IO ()
main = defaultMain . localOption (QuickCheckTests 1_000) . localOption (QuickCheckMaxSize 10_000) $ testGroup "rrb-vector"
    [ properties
    , strictness
    ]
