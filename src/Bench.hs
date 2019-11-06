module Bench where

import Criterion.Main
import qualified Free as F
import qualified FreeCoyo as FC

main :: IO ()
main = defaultMain
  [ bgroup "filter"
    [ bench "FreeT" $ nfIO (F.filterBench 10000000)
    , bench "FreeT - Coyo" $ nfIO (FC.filterBench 10000000)
    ]
  ]
