module Benchmark where

import Data.Tree.AVL ()

arbitraryIntVectorN :: Int -> IO ([Int], [Int])
arbitraryIntVectorN n = do
  xs <- arbitraryIntVectorOf n
  ys <- arbitraryIntVectorOf n
  return (xs, ys)

main :: IO ()
main =
  defaultMainWith
  (defaultConfig {reportFile = Just "fingertree-vs-avl.html"})
  [env (arbitraryIntVectorsOf 100)
        (\ ~(xs, ys) ->
          bgroup
            "fingertree versus avl fromlist/100"
            [bench "fingertree" $ nf FingerTree.fromList xs
            ,bench "avl" $ nf AVL.asTreeL ys]
  ,env (arbitraryIntVectorsOf 1000)
        (\ ~(xs, ys) ->
          bgroup
            "fingertree versus avl fromlist/1000"
            [bench "fingertree" $ nf FingerTree.fromList xs
            ,bench "avl" $ nf AVL.asTreeL ys]
  ,env (arbitraryIntVectorsOf 10000)
        (\ ~(xs, ys) ->
          bgroup
            "fingertree versus avl fromlist/10000"
            [bench "fingertree" $ nf FingerTree.fromList xs
            ,bench "avl" $ nf AVL.asTreeL ys]])]
            