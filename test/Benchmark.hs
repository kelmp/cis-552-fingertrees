module Benchmark where

import Data.FingerTree as FTlib ()
import Data.List as List ()
import FingerTree as FT
import Criterion.Main (defaultMainWith, defaultConfig, env, bgroup, bench, nf)
import Criterion.Types (reportFile)
import Test.QuickCheck (generate, vectorOf, arbitrary)
import Prelude as P

arbitraryIntVectorOf :: Int -> IO [Int]
arbitraryIntVectorOf n = generate $ vectorOf n arbitrary

arbitraryIntVectorsN :: Int -> IO ([Int], [Int])
arbitraryIntVectorsN n = do
  xs <- arbitraryIntVectorOf n
  ys <- arbitraryIntVectorOf n
  return (xs, ys)

arbFTrees :: Int -> Int -> [FT.FingerTree Int] -> IO [FT.FingerTree Int]
arbFTrees _ 0 l = return l
arbFTrees len nTrees l = do
  v <- arbitraryIntVectorOf len
  let l' = FT.fromList v : l in
    arbFTrees len (nTrees - 1) l'

arbLists :: Int -> Int -> [[Int]] -> IO [[Int]]
arbLists _ 0 l = return l
arbLists len nLists l = do
  v <- arbitraryIntVectorOf len
  let l' = v : l in
    arbLists len (nLists - 1) l'

arbFTreesAndLists :: Int -> Int -> Int -> IO ([FT.FingerTree Int], [[Int]])
arbFTreesAndLists len nTrees nLists = do
  aft <- arbFTrees len nTrees []
  al <- arbLists len nLists []
  return (aft, al)

listBench :: IO ()
listBench =
  defaultMainWith
    (defaultConfig {reportFile = Just "ft-vs-list.html"})
    [ env
        (arbFTreesAndLists 1000 1 2)
        ( \ ~(fts, ls) ->
            bgroup
              "ft O(1) v. list O(n) repeat insertTail: 1000 + 1 (x1000)"
              [ bench "fingertree" $
                  nf (foldr FT.insertTail (P.head fts)) (ls P.!! 1),
                bench "list" $
                  nf (foldr (\x acc -> acc ++ [x]) (P.head ls)) (ls P.!! 1)
              ]
        ),
      env
        (arbFTreesAndLists 100000 2 2)
        ( \ ~(fts, ls) ->
            bgroup
              "ft O(log n) v. list O(n) append: 100000 + 100000"
              [ bench "fingertree" $ nf (FT.append (P.head fts)) (fts P.!! 1),
                bench "list" $ nf (P.head ls ++) (ls P.!! 1)
              ]
        ),
      env
        (arbFTreesAndLists 1000 1 1)
        ( \ ~(fts, ls) ->
            bgroup
              "ft O(log n) v. list O(1) repeat tail: 1000"
              [ bench "fingertree" $ let ft = P.head fts in
                  nf (foldr (\_ acc -> FT.tail acc) ft) ft,
                bench "list" $ let l = P.head ls in
                  nf (foldr (\_ acc -> P.tail acc) l) l 
              ]
        ),
      env
        (arbFTreesAndLists 100000 1 1)
        ( \ ~(fts, ls) ->
            bgroup
              "ft O(1) v. list O(n) last: 100000"
              [ bench "fingertree" $ nf FT.last $ P.head fts,
                bench "list" $ nf P.last $ P.head ls
              ]
        )
    ]