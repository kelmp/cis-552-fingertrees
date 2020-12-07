module Benchmark where

import Data.FingerTree as FTlib ()
import Data.List as List ()
import FingerTree as FT ()

arbitraryIntVectorsN :: Int -> IO ([Int], [Int])
arbitraryIntVectorsN n = do
  xs <- arbitraryIntVectorOf n
  ys <- arbitraryIntVectorOf n
  return (xs, ys)

arbFTreesAndLists :: Int -> IO ([FingerTree Int], [[Int]])
arbFTreesAndLists len nTrees nLists = do
  fTrees <-
    [ ft | ft <- FT.fromList $ arbitraryIntVectorOf len, i <- [1 .. nTrees]
    ]
  lists <- [l | l <- arbitraryIntVectorOf len, i <- [1 .. nLists]]
  return (fTrees, lists)

listBench :: IO ()
listBench =
  defaultMainWith
    (defaultConfig {reportFile = Just "fingertree-vs-list.html"})
    [ env
        (arbFTreesAndLists 1000 1 2)
        ( \ ~(fts, ls) ->
            bgroup
              "fingertree versus list repeated append/1000 + 1000"
              [ bench "fingertree" $ nf $ foldr FT.insertTail fts [0] ls [1],
                bench "list" $ nf $ foldr (\x acc -> acc ++ [x]) ls [0] ls [1]
              ]
        ),
      env
        (arbFTreesAndLists 1000 2 2)
        ( \ ~(fts, ls) ->
            bgroup
              "fingertree versus list concat/1000 + 1000"
              [ bench "fingertree" $ nf $ FT.append fts [0] fts [1],
                bench "list" $ nf $ ls [1] ++ ls [2]
              ]
        )
    ]