import Test.Tasty
import Test.Tasty.QuickCheck

import Properties (properties)
import Strictness (strictness)

main :: IO ()
main = defaultMain . localOption (QuickCheckTests 1000) . localOption (QuickCheckMaxSize 10000) $ testGroup "rrb-vector"
    [ properties
    , strictness
    ]
