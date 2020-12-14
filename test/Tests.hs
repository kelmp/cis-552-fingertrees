{-# LANGUAGE ExtendedDefaultRules #-}

import Data.FingerTree as FT
import Sequence
import PriorityQueue
import Data.Maybe as Maybe
import FingerTree
-- ( FingerTree.FingerTree (..),
--   append,
--   concat,
--   fromList,
--   head,
--   insertHead,
--   insertTail,
--   isEmpty,
--   last,
--   removeLast,
--   split,
--   tail,
--   toList,
-- )
import Test.HUnit
import Test.QuickCheck

default (Int)

main :: IO ()
main = testAll

testAll :: IO ()
testAll = do
  allHUnit
  allQuickCheck
  putStrLn ""

allHUnit :: IO ()
allHUnit = do
  _ <-
    runTestTT
      ( TestList
          [ tToList,
            tInsertHead,
            tInsertTail,
            tLastTest,
            tRemoveTail,
            tHead,
            tTail,
            tIsEmpty,
            tFromList,
            tLength,
            tSplitSome,
            tSplitTree,
            tSplit
          ]
      )
  putStrLn ""

allQuickCheck :: IO ()
allQuickCheck = qcPostConditions >> qcMetamorphic >> qcModel

quickCheckN :: Test.QuickCheck.Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith $ stdArgs {maxSuccess = n, maxSize = 100}

verboseCheckN :: Test.QuickCheck.Testable prop => Int -> prop -> IO ()
verboseCheckN n = verboseCheckWith $ stdArgs {maxSuccess = n, maxSize = 100}

ftEmpty :: FingerTree.FingerTree Int
ftEmpty = Nil

ft1 :: FingerTree.FingerTree Int
ft1 = Unit 1

ft2 :: FingerTree.FingerTree Int
ft2 = More 2 (One 1) Nil (One 2)

ft3 :: FingerTree.FingerTree Int
ft3 = More 3 (One 1) Nil (Two 2 3)

ft4 :: FingerTree.FingerTree Int
ft4 = More 4 (One 1) Nil (Three 2 3 4)

ft5 :: FingerTree.FingerTree Int
ft5 = More 5 (One 1) (Unit (Pair 2 2 3)) (Two 4 5)

ft6 :: FingerTree.FingerTree Int
ft6 = More 6 (One 1) (Unit (Pair 2 2 3)) (Three 4 5 6)

ft7 :: FingerTree.FingerTree Int
ft7 = More 7 (One 1) (More 4 (One (Pair 2 2 3)) Nil (One (Pair 2 4 5))) (Two 6 7)

ft8 :: FingerTree.FingerTree Int
ft8 = More 8 (One 1) (More 4 (One (Pair 2 2 3)) Nil (One (Pair 2 4 5))) (Three 6 7 8)

ft9 :: FingerTree.FingerTree Int
ft9 =
  More 9 (One 1) (More 6 (One (Pair 2 2 3)) Nil (Two (Pair 2 4 5) (Pair 2 6 7))) (Two 8 9)

tInsertHead :: Test
tInsertHead =
  TestList
    [ "Insert head 0 -> 1" ~: insertHead (1 :: Int) Nil ~?= Unit 1,
      "Insert head 1 -> 2" ~: insertHead (2 :: Int) (Unit 1)
        ~?= More 2 (One 2) Nil (One 1),
      "Insert head 4 -> 5" ~: insertHead (5 :: Int) (More 4 (Three 4 3 2) Nil (One 1))
        ~?= More 5 (Two 5 4) (Unit (Pair 2 3 2)) (One 1),
      "Insert head 6 -> 7"
        ~: insertHead (7 :: Int) (More 6 (Three 6 5 4) (Unit (Pair 2 3 2)) (One 1))
        ~?= More 7 (Two 7 6) (More 4 (One (Pair 2 5 4)) Nil (One (Pair 2 3 2))) (One 1),
      "Insert head 8 -> 9"
        ~: insertHead
          (9 :: Int)
          ( More
              8
              (Three 8 7 6)
              (More 4 (One (Pair 2 5 4)) Nil (One (Pair 2 3 2)))
              (One 1)
          )
        ~?= More
          9
          (Two 9 8)
          ( More
              6
              (Two (Pair 2 7 6) (Pair 2 5 4))
              Nil
              (One (Pair 2 3 2))
          )
          (One 1)
    ]

tTail :: Test
tTail =
  TestList [
    "Tail 0 -> 0" ~: FingerTree.tail Nil ~?= Nil,
    "Tail 1 -> 0" ~: FingerTree.tail ft1 ~?= Nil,
    "Tail 2 -> 1" ~: toList (FingerTree.tail ft2) ~?= [2],
    "Tail 3 -> 2" ~: toList (FingerTree.tail ft3) ~?= [2,3],
    "Tail 4 -> 3" ~: toList (FingerTree.tail ft4) ~?= [2,3,4],
    "Tail 5 -> 4" ~: toList (FingerTree.tail ft5) ~?= [2,3,4,5],
    "Tail 6 -> 5" ~: toList (FingerTree.tail ft6) ~?= [2,3,4,5,6],
    "Tail 7 -> 6" ~: toList (FingerTree.tail ft7) ~?= [2,3,4,5,6,7],
    "Tail 8 -> 7" ~: toList (FingerTree.tail ft8) ~?= [2,3,4,5,6,7,8],
    "Tail 9 -> 8" ~: toList (FingerTree.tail ft9) ~?= [2,3,4,5,6,7,8,9]
  ]

tInsertTail :: Test
tInsertTail =
  TestList
    [ "Insert tail 0 -> 1" ~: insertTail 1 ftEmpty ~?= ft1,
      "Insert tail 1 -> 2" ~: insertTail 2 ft1 ~?= ft2,
      "Insert tail 3 -> 4" ~: insertTail 4 ft3 ~?= ft4,
      "Insert tail 4 -> 5" ~: insertTail 5 ft4 ~?= ft5,
      "Insert tail 5 -> 6" ~: insertTail 6 ft5 ~?= ft6,
      "Insert tail 6 -> 7" ~: insertTail 7 ft6 ~?= ft7,
      "Insert tail 7 -> 8" ~: insertTail 8 ft7 ~?= ft8,
      "Insert tail 8 -> 9" ~: insertTail 9 ft8 ~?= ft9
    ]

tHead :: Test
tHead =
  TestList
    [ "Head empty" ~: FingerTree.head ftEmpty ~?= Nothing,
      "Head unit" ~: FingerTree.head ft1 ~?= Just 1,
      "Head more (one ...)" ~: FingerTree.head ft9 ~?= Just 1,
      "Head more (two ....)" ~: FingerTree.head (insertHead 0 ft9) ~?= Just 0,
      "Head more (three ....)"
        ~: FingerTree.head (insertHead (-1) (insertHead 0 ft9)) ~?= Just (-1)
    ]

tLastTest :: Test
tLastTest =
  TestList
    [ "Last empty" ~: FingerTree.last ftEmpty ~?= Nothing,
      "Last 1" ~: FingerTree.last ft1 ~?= Just 1,
      "Last 2" ~: FingerTree.last ft2 ~?= Just 2,
      "Last 3" ~: FingerTree.last ft3 ~?= Just 3,
      "Last 4" ~: FingerTree.last ft4 ~?= Just 4,
      "Last 5" ~: FingerTree.last ft5 ~?= Just 5,
      "Last 6" ~: FingerTree.last ft6 ~?= Just 6,
      "Last 7" ~: FingerTree.last ft7 ~?= Just 7,
      "Last 8" ~: FingerTree.last ft8 ~?= Just 8,
      "Last 9" ~: FingerTree.last ft9 ~?= Just 9
    ]

tRemoveTail :: Test
tRemoveTail =
  TestList
    [ "Remove Tail Empty" ~: removeLast ftEmpty ~?= ftEmpty,
      "Remove Tail 1" ~: removeLast ft1 ~?= ftEmpty,
      "Remove Tail 2" ~: removeLast ft2 ~?= ft1,
      "Remove Tail 3" ~: removeLast ft3 ~?= ft2,
      "Remove Tail 4" ~: removeLast ft4 ~?= ft3,
      "Remove Tail 5" ~: removeLast ft5 ~?= ft4,
      "Remove Tail 6" ~: removeLast ft6 ~?= ft5,
      "Remove Tail 7" ~: removeLast ft7 ~?= ft6,
      "Remove Tail 8" ~: removeLast ft8 ~?= ft7,
      "Remove Tail 9" ~: removeLast ft9 ~?= ft8
    ]

tIsEmpty :: Test
tIsEmpty =
  TestList
    [ "Empty is empty" ~: isEmpty ftEmpty ~?= True,
      "Nonempty is not empty"
        ~: all isEmpty [ft1, ft2, ft3, ft4, ft5, ft6, ft7, ft8, ft9]
        ~?= False
    ]

tSplitSome :: Test
tSplitSome =
  TestList
    [ "splitSome singleton" ~: splitSome 0 (One 3) ~?= Split Nothing 3 Nothing,
      "splitSome Two at 0 (no split)"
        ~: splitSome 0 (Two 3 4) ~?= Split Nothing 3 (Just (One 4)),
      "splitSome Two at 1"
        ~: splitSome 1 (Two 3 4) ~?= Split (Just (One 3)) 4 Nothing,
      "splitSome Three at 0 (no split)"
        ~: splitSome 0 (Three 7 8 9)
        ~?= Split Nothing 7 (Just (Two 8 9)),
      "splitSome Three at 1"
        ~: splitSome 1 (Three 7 8 9) ~?= Split (Just (One 7)) 8 (Just (One 9)),
      "splitSome Three at 2"
        ~: splitSome 2 (Three 7 8 9) ~?= Split (Just (Two 7 8)) 9 Nothing
    ]

tSplitTree :: Test
tSplitTree =
  TestList
    [ "splitTree len 1" ~: splitTree 0 ft1 ~?= Split Nil 1 Nil,
      "splitTree len 2 at 0" ~: splitTree 0 ft2 ~?= Split Nil 1 (Unit 2),
      "splitTree len 2 at 1" ~: splitTree 1 ft2 ~?= Split (Unit 1) 2 Nil,
      "splitTree len 3 at 0"
        ~: splitTree 0 ft3 ~?= Split Nil 1 (more (One 2) Nil (One 3)),
      "splitTree len 3 at 1"
        ~: splitTree 1 ft3 ~?= Split (Unit 1) 2 (Unit 3),
      "splitTree len 3 at 2"
        ~: splitTree 2 ft3 ~?= Split (more (One 1) Nil (One 2)) 3 Nil,
      "splitTree len 4 at 0"
        ~: splitTree 0 ft4 ~?= Split Nil 1 (more (One 2) Nil (Two 3 4)),
      "splitTree len 4 at 1"
        ~: splitTree 1 ft4 ~?= Split (Unit 1) 2 (more (One 3) Nil (One 4)),
      "splitTree len 4 at 2"
        ~: splitTree 2 ft4 ~?= Split (more (One 1) Nil (One 2)) 3 (Unit 4),
      "splitTree len 4 at 3"
        ~: splitTree 3 ft4 ~?= Split (more (One 1) Nil (Two 2 3)) 4 Nil
    ]

tupleMap :: (a, a) -> (a -> b) -> (b, b)
tupleMap (x, y) f = (f x, f y)

tSplit :: Test
tSplit =
  TestList
    ["split empty index in bounds" ~: FingerTree.split 0 ftEmpty ~?= (Nil, Nil),
     "split empty index out of bounds" ~: FingerTree.split 10 ftEmpty ~?= (Nil, Nil),
     "split len 1, split at 0" ~: FingerTree.split 0 ft1 ~?= (Nil, Unit 1),
     "split len 1, split at 1" ~: FingerTree.split 1 ft1 ~?=  (Unit 1, Nil),
     "split len 1, split out of bounds" ~: FingerTree.split 2 ft1 ~?=  (Unit 1, Nil),
     "split len 2, split at 0" ~: tupleMap (FingerTree.split 0 ft2) FingerTree.toList ~?= ([], [1,2]),
     "split len 2, split at 1" ~: tupleMap (FingerTree.split 1 ft2) FingerTree.toList ~?= ([1], [2]),
     "split len 2, split at 2" ~: tupleMap (FingerTree.split 2 ft2) FingerTree.toList ~?= ([1,2], []),
     "split len 2, split at 5" ~: tupleMap (FingerTree.split 5 ft2) FingerTree.toList ~?= ([1,2], []),
     "split len 3, split at 0" ~: tupleMap (FingerTree.split 0 ft3) FingerTree.toList ~?= ([], [1,2,3]),
     "split len 3, split at 1" ~: tupleMap (FingerTree.split 1 ft3) FingerTree.toList ~?= ([1], [2,3]),
     "split len 3, split at 2" ~: tupleMap (FingerTree.split 2 ft3) FingerTree.toList ~?= ([1,2], [3]),
     "split len 4, split at 0" ~: tupleMap (FingerTree.split 0 ft4) FingerTree.toList ~?= ([], [1,2,3,4]),
     "split len 4, split at 2" ~: tupleMap (FingerTree.split 2 ft4) FingerTree.toList ~?= ([1,2], [3,4]),
     "split len 4, split at 3" ~: tupleMap (FingerTree.split 3 ft4) FingerTree.toList ~?= ([1,2,3], [4]),
     "split len 5, split at 0" ~: tupleMap (FingerTree.split 0 ft5) FingerTree.toList ~?= ([], [1,2,3,4,5]),
     "split len 5, split at 3" ~: tupleMap (FingerTree.split 3 ft5) FingerTree.toList ~?= ([1,2,3], [4,5]),
     "split len 6, split at 0" ~: tupleMap (FingerTree.split 0 ft6) FingerTree.toList ~?= ([], [1,2,3,4,5,6]),
     "split len 6, split at 4" ~: tupleMap (FingerTree.split 4 ft6) FingerTree.toList ~?= ([1,2,3,4],[5,6]),
     "split len 7, split at 0" ~: tupleMap (FingerTree.split 0 ft7) FingerTree.toList ~?= ([], [1,2,3,4,5,6,7]),
     "split len 7, split at 5" ~: tupleMap (FingerTree.split 5 ft7) FingerTree.toList ~?= ([1,2,3,4,5],[6,7]),
     "split len 8, split at 0" ~: tupleMap (FingerTree.split 0 ft8) FingerTree.toList ~?= ([], [1,2,3,4,5,6,7,8]),
     "split len 8, split at 3" ~: tupleMap (FingerTree.split 3 ft8) FingerTree.toList ~?= ([1,2,3], [4,5,6,7,8]),
     "split len 8, split at 5" ~: tupleMap (FingerTree.split 5 ft8) FingerTree.toList ~?= ([1,2,3,4,5], [6,7,8]),
     "split len 9, split at 0" ~: tupleMap (FingerTree.split 0 ft9) FingerTree.toList ~?= ([], [1,2,3,4,5,6,7,8,9]),
     "split len 9, split at 1" ~: tupleMap (FingerTree.split 1 ft9) FingerTree.toList ~?= ([1],[2,3,4,5,6,7,8,9]),
     "split len 9, split at 2" ~: tupleMap (FingerTree.split 2 ft9) FingerTree.toList ~?= ([1,2],[3,4,5,6,7,8,9]),
     "split len 9, split at 3" ~: tupleMap (FingerTree.split 3 ft9) FingerTree.toList ~?= ([1,2,3],[4,5,6,7,8,9]),
     "split len 9, split at 4" ~: tupleMap (FingerTree.split 4 ft9) FingerTree.toList ~?= ([1,2,3,4],[5,6,7,8,9]),
     "split len 9, split at 5" ~: tupleMap (FingerTree.split 5 ft9) FingerTree.toList ~?= ([1,2,3,4,5],[6,7,8,9]),
     "split len 9, split at 6" ~: tupleMap (FingerTree.split 6 ft9) FingerTree.toList ~?= ([1,2,3,4,5,6],[7,8,9]),
     "split len 9, split at 7" ~: tupleMap (FingerTree.split 7 ft9) FingerTree.toList ~?= ([1,2,3,4,5,6,7],[8,9]),
     "split len 9, split at 8" ~: tupleMap (FingerTree.split 8 ft9) FingerTree.toList ~?= ([1,2,3,4,5,6,7,8],[9]),
     "split len 9, split at 9" ~: tupleMap (FingerTree.split 9 ft9) FingerTree.toList ~?= ([1,2,3,4,5,6,7,8,9],[]),
     "split len 9, split at 10" ~: tupleMap (FingerTree.split 10 ft9) FingerTree.toList ~?= ([1,2,3,4,5,6,7,8,9],[])
    ]

tToList :: Test
tToList =
  TestList
    [ "toList empty" ~: toList ftEmpty ~?= [],
      "toList 1" ~: toList ft1 ~?= [1],
      "toList 2" ~: toList ft2 ~?= [1, 2],
      "toList 3" ~: toList ft3 ~?= [1, 2, 3],
      "toList 4" ~: toList ft4 ~?= [1, 2, 3, 4],
      "toList 5" ~: toList ft5 ~?= [1, 2, 3, 4, 5],
      "toList 6" ~: toList ft6 ~?= [1, 2, 3, 4, 5, 6],
      "toList 7" ~: toList ft7 ~?= [1, 2, 3, 4, 5, 6, 7],
      "toList 8" ~: toList ft8 ~?= [1, 2, 3, 4, 5, 6, 7, 8],
      "toList 9" ~: toList ft9 ~?= [1, 2, 3, 4, 5, 6, 7, 8, 9]
    ]

tFromList :: Test
tFromList =
  TestList
    [ "fromList empty" ~: ftEmpty ~?= FingerTree.fromList [],
      "fromList 1" ~: ft1 ~?= FingerTree.fromList [1],
      "fromList 2" ~: FingerTree.toList ft2 ~?= toList (FingerTree.fromList [1, 2]),
      "fromList 3" ~: FingerTree.toList ft3 ~?= toList (FingerTree.fromList [1, 2, 3]),
      "fromList 4" ~: FingerTree.toList ft4 ~?= toList (FingerTree.fromList [1, 2, 3, 4]),
      "fromList 5" ~: FingerTree.toList ft5 ~?= toList (FingerTree.fromList [1, 2, 3, 4, 5]),
      "fromList 6" ~: FingerTree.toList ft6 ~?= toList (FingerTree.fromList [1, 2, 3, 4, 5, 6]),
      "fromList 7" ~: FingerTree.toList ft7 ~?= toList (FingerTree.fromList [1, 2, 3, 4, 5, 6, 7]),
      "fromList 8" ~: FingerTree.toList ft8 ~?= toList (FingerTree.fromList [1, 2, 3, 4, 5, 6, 7, 8]),
      "fromList 9" ~: FingerTree.toList ft9 ~?= toList (FingerTree.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9])
    ]

tLength :: Test
tLength =
  TestList
    [ "Length empty" ~: FingerTree.measure ftEmpty ~?= 0,
      "Length 1" ~: FingerTree.measure ft1 ~?= 1,
      "Length 2" ~: FingerTree.measure ft2 ~?= 2,
      "Length 3" ~: FingerTree.measure ft3 ~?= 3,
      "Length 4" ~: FingerTree.measure ft4 ~?= 4,
      "Length 5" ~: FingerTree.measure ft5 ~?= 5,
      "Length 6" ~: FingerTree.measure ft6 ~?= 6,
      "Length 7" ~: FingerTree.measure ft7 ~?= 7,
      "Length 8" ~: FingerTree.measure ft8 ~?= 8,
      "Length 9" ~: FingerTree.measure ft9 ~?= 9
    ]

--------------- QuickCheck Properties ---------------

-- (1) PostCondition (Invariants) Properties

qcPostConditions :: IO ()
qcPostConditions = qc1 >> qc2 >> qc3 >> qc4 >> qc5 >> qc6 >> qc7

-- a. insertHead
prop_insertHeadHead :: FingerTree.Measured a => Eq a => FingerTree.FingerTree a -> a -> Bool
prop_insertHeadHead t x =
  let newTree = insertHead x t
   in case FingerTree.head newTree of
        Nothing -> False
        Just v -> v == x

prop_insertHeadFirst :: FingerTree.Measured a => Eq a => FingerTree.FingerTree a -> a -> Bool
prop_insertHeadFirst t x =
  let newTree = insertHead x t
   in Prelude.head (toList newTree) == x

-- b. insert tail
prop_insertTailTail :: FingerTree.Measured a => Eq a => FingerTree.FingerTree a -> a -> Bool
prop_insertTailTail t x =
  let newTree = insertTail x t
   in case FingerTree.last newTree of
        Nothing -> False
        Just v -> v == x

prop_insertTailLast :: FingerTree.Measured a => Eq a => FingerTree.FingerTree a -> a -> Bool
prop_insertTailLast t x =
  let newTree = insertTail x t
   in Prelude.last (toList newTree) == x

-- e. removeLast
prop_removeTailChanged :: FingerTree.Measured a => Eq a => FingerTree.FingerTree a -> Bool
prop_removeTailChanged t =
  let newTree = removeLast t
   in case t of
        Nil -> newTree == t
        _ -> newTree /= t

-- g. append
prop_append :: Eq a => FingerTree.Measured a => FingerTree.FingerTree a -> FingerTree.FingerTree a -> Bool
prop_append t1 t2 =
  let t = append t1 t2
   in FingerTree.head t1 == FingerTree.head t
        && FingerTree.last t2 == FingerTree.last t
        && toList t == toList t1 ++ toList t2

-- h. split
prop_split :: FingerTree.Measured a => Eq a => Int -> FingerTree.FingerTree a -> Bool
prop_split x t =
  let (t1, t2) = FingerTree.split x t
   in FingerTree.head t == FingerTree.head t1
        && FingerTree.last t1 == FingerTree.last t2

qc1 :: IO ()
qc1 = quickCheck (prop_insertHeadHead :: FingerTree.FingerTree Int -> Int -> Bool)

qc2 :: IO ()
qc2  = quickCheck (prop_insertHeadFirst :: FingerTree.FingerTree Int -> Int -> Bool)

qc3 :: IO ()
qc3  = quickCheck (prop_insertTailTail :: FingerTree.FingerTree Int -> Int -> Bool)

qc4 :: IO ()
qc4  = quickCheck (prop_insertTailLast :: FingerTree.FingerTree Int -> Int -> Bool)

qc5 :: IO ()
qc5  = quickCheck (prop_removeTailChanged :: FingerTree.FingerTree Int -> Bool)

qc6 :: IO ()
qc6  = quickCheck (prop_append :: FingerTree.FingerTree Int -> FingerTree.FingerTree Int -> Bool)

qc7 :: IO ()
qc7  = quickCheck (prop_split :: Int -> FingerTree.FingerTree Int -> Bool)

-- (2) Metamorphic Properties (Bulk of the testing):

qcMetamorphic :: IO ()
qcMetamorphic = qc8 >> qc9 >> qc10 >> qc11 >> qc12 >> qc13

-- a.  insert at tail and then remove tail, should be previous tail
prop_insertRemoveTail :: FingerTree.Measured a => Eq a => FingerTree.FingerTree a -> a -> Bool
prop_insertRemoveTail t x =
  let t' = removeLast (insertTail x t)
   in FingerTree.last t == FingerTree.last t'

-- b. insert at tail twice and then remove tail, tails should be second tail
prop_insertTwice :: FingerTree.Measured a => Eq a => FingerTree.FingerTree a -> a -> a -> Bool
prop_insertTwice t x y =
  let t' = removeLast (insertTail y (insertTail x t))
   in FingerTree.last t' == Just x

-- c. insert twice and then remove to check that they are still there
prop_insertRemoveTwice :: FingerTree.Measured a => Eq a => FingerTree.FingerTree a -> a -> a -> Bool
prop_insertRemoveTwice t x y =
  let t' = removeLast (removeLast (insertTail y (insertTail x t)))
   in FingerTree.last t' == FingerTree.last t

-- d. isempty -> insert -> should no longer be empty
prop_isEmptyInsert :: FingerTree.Measured a => Eq a => FingerTree.FingerTree a -> a -> Bool
prop_isEmptyInsert t x = not (isEmpty (insertHead x t))

-- e. insert head, insert tail and append trees
prop_insertAppend :: FingerTree.Measured a => Eq a => FingerTree.FingerTree a -> FingerTree.FingerTree a -> a -> a -> Bool
prop_insertAppend t1 t2 x1 x2 =
  let t1' = insertHead x1 t1
   in let t2' = insertTail x2 t2
       in let t' = append t1' t2'
           in FingerTree.head t' == FingerTree.head t1'
                && FingerTree.last t' == FingerTree.last t2'
                && toList t' == toList t1' ++ toList t2'

-- f. append after splitting
prop_SplitAppend :: FingerTree.Measured a => Eq a => FingerTree.FingerTree a -> Int -> Bool
prop_SplitAppend t x = let (t1, t2) = FingerTree.split x t in
  t == append t1 t2

qc8 :: IO ()
qc8 = quickCheck (prop_insertRemoveTail :: FingerTree.FingerTree Int -> Int -> Bool)

qc9 :: IO ()
qc9  = quickCheck (prop_insertTwice :: FingerTree.FingerTree Int -> Int -> Int -> Bool)

qc10 :: IO ()
qc10  = quickCheck (prop_insertRemoveTwice :: FingerTree.FingerTree Int -> Int -> Int -> Bool)

qc11 :: IO ()
qc11  = quickCheck (prop_isEmptyInsert :: FingerTree.FingerTree Int -> Int -> Bool)

qc12 :: IO ()
qc12  = quickCheck (prop_insertAppend :: FingerTree.FingerTree Int -> FingerTree.FingerTree Int -> Int -> Int -> Bool)

qc13 :: IO ()
qc13  = quickCheck (prop_SplitAppend :: FingerTree.FingerTree Int -> Int -> Bool)

-- (3) Model-Based Poperties (could just use lists for now?)

qcModel :: IO ()
qcModel = qc14 >> qc15 >> qc16 >> qc17 >> qc18 >> qc19 >> qc20 >> qc21

prop_modelInsertHead :: FingerTree.Measured a => Eq a => [a] -> Bool
prop_modelInsertHead l =
  let myTree = foldr insertHead Nil l
   in let modelTree = l
       in FingerTree.toList myTree == modelTree

prop_modelInsertTail :: FingerTree.Measured a => Eq a => [a] -> Bool
prop_modelInsertTail l =
  let myTree = foldr insertTail Nil l
   in let modelTree = l
       in FingerTree.toList myTree == modelTree

prop_modelFromListHead :: FingerTree.Measured a => Eq a => [a] -> Bool
prop_modelFromListHead l =
  let myTree = FingerTree.fromList l
   in let modelTree = l
       in FingerTree.head myTree == getFirst modelTree

prop_modelFromListTail :: FingerTree.Measured a => Eq a => [a] -> Bool
prop_modelFromListTail l =
  let myTree = FingerTree.fromList l
   in let modelTree = l
       in FingerTree.last myTree == getLast modelTree

getFirst :: [a] -> Maybe a
getFirst (x : xs) = Just x
getFirst [] = Nothing

getLast :: [a] -> Maybe a
getLast [x] = Just x
getLast (x : xs) = getLast xs
getLast [] = Nothing

prop_modelFromListEveryElement :: FingerTree.Measured a => Eq a => [a] -> Bool
prop_modelFromListEveryElement l =
  let myTree = FingerTree.fromList l
   in let modelTree = l
       in toList myTree == modelTree

prop_modelFromListEveryElementAppend :: FingerTree.Measured a => Eq a => [a] -> [a] -> Bool
prop_modelFromListEveryElementAppend l1 l2 =
  let myTree = append (FingerTree.fromList l1) (FingerTree.fromList l2)
   in let modelTree = l1 ++ l2
       in toList myTree == modelTree

prop_modelSplit :: FingerTree.Measured a => Eq a => Int -> [a] -> Bool
prop_modelSplit x l = 
  let t = FingerTree.fromList l in
    let (t1, t2) = FingerTree.split x t  in
      toList t1 == take x l && toList t2 == drop x l

prop_modelAppend :: FingerTree.FingerTree Int -> FingerTree.FingerTree Int -> Bool
prop_modelAppend t1 t2 = toList (t1 <> t2) == toList t1 ++ toList t2

qc14 :: IO ()
qc14 = quickCheck (prop_modelInsertHead :: [Int] -> Bool)

qc15 :: IO ()
qc15  = quickCheck (prop_modelInsertTail ::[Int] -> Bool)

qc16 :: IO ()
qc16  = quickCheck (prop_modelFromListHead :: [Int] -> Bool)

qc17 :: IO ()
qc17  = quickCheck (prop_modelFromListTail :: [Int] -> Bool)

qc18 :: IO ()
qc18  = quickCheck (prop_modelFromListEveryElement :: [Int] -> Bool)

qc19 :: IO ()
qc19  = quickCheck (prop_modelFromListEveryElementAppend :: [Int] -> [Int] -> Bool)

qc20 :: IO ()
qc20  = quickCheck (prop_modelSplit :: Int -> [Int] -> Bool)

qc21 :: IO ()
qc21  = quickCheck (prop_modelAppend :: FingerTree.FingerTree Int -> FingerTree.FingerTree Int -> Bool)

qcFingerTree :: IO ()
qcFingerTree = putStrLn "I am in Missouri"


--------------- Sequence Tsts --------- ------

seqEmpty :: Sequence a
SeqEmpty = Sequence.empty

seq1 :: Sequence a
seq1 = Sequence.singleton 1 

seq2 :: Sequence a
seq2 = seq1 |> 2

seq3 :: Sequence a
seq3 = seq2 |> 3

seq4 :: Sequence a
seq4 = seq3 |> 4

seq5 :: Sequence a
seq5 = seq4 |> 5

tSeqUnit :: Test
tSeqUnit =
  TestList
    [ "Seq empty" ~: Sequence.toList seqEmpty ~?= [],
      "Seq 1" ~: Sequence.toList seq1 ~?= [1],
      "Seq 2" ~: Sequence.toList seq2 ~?= [1,2],
      "Seq 3" ~: Sequence.toList seq3 ~?= [1,2,3],
      "Seq 4" ~: Sequence.toList seq4 ~?= [1,2,3,4],
      "Seq 4" ~: Sequence.toList seq5 ~?= [1,2,3,4,5]
    ]

-- QuickCheck PostCondition Properties:
prop_postCondtionDeleteAt :: Sequence Int -> Int -> Bool
prop_postCondtionDeleteAt s i = let oldSize = size s in
  (oldSize - 1) == size (Sequence.deleteAt i s)

prop_postConditionInsertHead :: Sequence Int -> Int -> Bool
prop_postConditionInsertHead s x = let s1 = x Sequence.<| s in
  first s1 == Just x

prop_postConditionInsertTail :: Sequence Int -> Int -> Bool
prop_postConditionInsertTail s x = let s2 = s Sequence.|> x in
  first s1 == Just x

-- QuickCheck Metamorphic Properties:

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = lft ++ rgt
  where (lft, (_ : rgt)) = splitAt idx xs

insertAt :: Int -> a -> [a] -> [a]
insertAt idx elt xs = lft ++ (elt : rgt)
  where (lft, rgt) = splitAt idx xs

prop_InsertAtLookUp :: Sequence Int -> Int -> Int -> Bool
prop_InsertAtLookUp s i x = let s1 = Sequence.insertAt i x s in
  if i < Sequence.length s then Sequence.lookup i s1 == Just x else Sequence.lookup i s1 == Nothing

prop_deleteAtLookUp :: [Int] -> Int -> Bool
prop_deleteAtLookUp l i x = 
  let s = Sequence.fromList l in
    let s1 = Sequence.deleteAt i l in
      Sequence.toList s1 == Main.deleteAt i l

prop_seqAppend :: Sequence Int -> Sequence Int -> Bool
prop_seqAppend s1 s2 = let s2 = s Sequence.|> x in
  first s1 == Just xM


----------- Priority Queue Tests -----------

pqEmpty :: PriorityQueue a
pqEmpty = PriorityQueue.empty

pq1 :: PriorityQueue a
pq1 = PriorityQueue.singleton 1 

pq2 :: PriorityQueue a
pq2 = enqueue pq1 2

pq3 :: PriorityQueue a
pq3 = enqueue pq2 3

pq4 :: PriorityQueue a
pq4 = enqueue pq3 4

pq5 :: PriorityQueue a
pq5 = enqueue pq4 5

tPQUnit :: Test
tPQUnit =
  TestList
    [ "PQ empty" ~: PriortiyQueue.toList pqEmpty ~?= [],
      "PQ 1" ~: PriortiyQueue.toList pq1 ~?= [1],
      "PQ 2" ~: PriortiyQueue.toList pq2 ~?= [1,2],
      "PQ 3" ~: PriortiyQueue.toList pq3 ~?= [1,2,3],
      "PQ 4" ~: PriortiyQueue.toList pq4 ~?= [1,2,3,4],
      "PQ 4" ~: SequePriortiyQueuence.toList pq5 ~?= [1,2,3,4,5]
    ]
