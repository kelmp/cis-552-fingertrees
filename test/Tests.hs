{-# LANGUAGE ExtendedDefaultRules #-}

import Data.FingerTree as FTLib
import Data.Maybe as Maybe
import FingerTree as FT
import PriorityQueue as PQ
import Sequence
-- ( FT.FingerTree (..),
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
  qcSequence
  qcPQ
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
            tSplit,
            tSeqUnit,
            tPQUnit
          ]
      )
  putStrLn ""

allQuickCheck :: IO ()
allQuickCheck = qcPostConditions >> qcMetamorphic >> qcModel

quickCheckN :: Test.QuickCheck.Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith $ stdArgs {maxSuccess = n, maxSize = 100}

verboseCheckN :: Test.QuickCheck.Testable prop => Int -> prop -> IO ()
verboseCheckN n = verboseCheckWith $ stdArgs {maxSuccess = n, maxSize = 100}

ftEmpty :: FT.FingerTree Int
ftEmpty = Nil

ft1 :: FT.FingerTree Int
ft1 = Unit 1

ft2 :: FT.FingerTree Int
ft2 = More 2 (One 1) Nil (One 2)

ft3 :: FT.FingerTree Int
ft3 = More 3 (One 1) Nil (Two 2 3)

ft4 :: FT.FingerTree Int
ft4 = More 4 (One 1) Nil (Three 2 3 4)

ft5 :: FT.FingerTree Int
ft5 = More 5 (One 1) (Unit (Pair 2 2 3)) (Two 4 5)

ft6 :: FT.FingerTree Int
ft6 = More 6 (One 1) (Unit (Pair 2 2 3)) (Three 4 5 6)

ft7 :: FT.FingerTree Int
ft7 = More 7 (One 1) (More 4 (One (Pair 2 2 3)) Nil (One (Pair 2 4 5))) (Two 6 7)

ft8 :: FT.FingerTree Int
ft8 = More 8 (One 1) (More 4 (One (Pair 2 2 3)) Nil (One (Pair 2 4 5))) (Three 6 7 8)

ft9 :: FT.FingerTree Int
ft9 =
  More
    9
    (One 1)
    ( More
        6
        (One (Pair 2 2 3))
        Nil
        (Two (Pair 2 4 5) (Pair 2 6 7))
    )
    (Two 8 9)

tInsertHead :: Test
tInsertHead =
  TestList
    [ "Insert head 0 -> 1" ~: insertHead (1 :: Int) Nil ~?= Unit 1,
      "Insert head 1 -> 2" ~: insertHead (2 :: Int) (Unit 1)
        ~?= More 2 (One 2) Nil (One 1),
      "Insert head 4 -> 5"
        ~: insertHead
          (5 :: Int)
          (More 4 (Three 4 3 2) Nil (One 1))
        ~?= More 5 (Two 5 4) (Unit (Pair 2 3 2)) (One 1),
      "Insert head 6 -> 7"
        ~: insertHead
          (7 :: Int)
          ( More
              6
              (Three 6 5 4)
              (Unit (Pair 2 3 2))
              (One 1)
          )
        ~?= More
          7
          (Two 7 6)
          ( More
              4
              (One (Pair 2 5 4))
              Nil
              (One (Pair 2 3 2))
          )
          (One 1),
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
  TestList
    [ "Tail 0 -> 0" ~: FT.tail Nil ~?= Nil,
      "Tail 1 -> 0" ~: FT.tail ft1 ~?= Nil,
      "Tail 2 -> 1" ~: FT.toList (FT.tail ft2) ~?= [2],
      "Tail 3 -> 2" ~: FT.toList (FT.tail ft3) ~?= [2, 3],
      "Tail 4 -> 3" ~: FT.toList (FT.tail ft4) ~?= [2, 3, 4],
      "Tail 5 -> 4" ~: FT.toList (FT.tail ft5) ~?= [2, 3, 4, 5],
      "Tail 6 -> 5" ~: FT.toList (FT.tail ft6) ~?= [2, 3, 4, 5, 6],
      "Tail 7 -> 6" ~: FT.toList (FT.tail ft7) ~?= [2, 3, 4, 5, 6, 7],
      "Tail 8 -> 7" ~: FT.toList (FT.tail ft8) ~?= [2, 3, 4, 5, 6, 7, 8],
      "Tail 9 -> 8" ~: FT.toList (FT.tail ft9) ~?= [2, 3, 4, 5, 6, 7, 8, 9]
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
    [ "Head empty" ~: FT.head ftEmpty ~?= Nothing,
      "Head unit" ~: FT.head ft1 ~?= Just 1,
      "Head more (one ...)" ~: FT.head ft9 ~?= Just 1,
      "Head more (two ....)" ~: FT.head (insertHead 0 ft9) ~?= Just 0,
      "Head more (three ....)"
        ~: FT.head (insertHead (-1) (insertHead 0 ft9)) ~?= Just (-1)
    ]

tLastTest :: Test
tLastTest =
  TestList
    [ "Last empty" ~: FT.last ftEmpty ~?= Nothing,
      "Last 1" ~: FT.last ft1 ~?= Just 1,
      "Last 2" ~: FT.last ft2 ~?= Just 2,
      "Last 3" ~: FT.last ft3 ~?= Just 3,
      "Last 4" ~: FT.last ft4 ~?= Just 4,
      "Last 5" ~: FT.last ft5 ~?= Just 5,
      "Last 6" ~: FT.last ft6 ~?= Just 6,
      "Last 7" ~: FT.last ft7 ~?= Just 7,
      "Last 8" ~: FT.last ft8 ~?= Just 8,
      "Last 9" ~: FT.last ft9 ~?= Just 9
    ]

tRemoveTail :: Test
tRemoveTail =
  TestList
    [ "Remove Tail Empty" ~: FT.toList (removeLast ftEmpty) ~?= [],
      "Remove Tail 1" ~: FT.toList (removeLast ft1) ~?= [],
      "Remove Tail 2" ~: FT.toList (removeLast ft2) ~?= [1],
      "Remove Tail 3" ~: FT.toList (removeLast ft3) ~?= [1 .. 2],
      "Remove Tail 4" ~: FT.toList (removeLast ft4) ~?= [1 .. 3],
      "Remove Tail 5" ~: FT.toList (removeLast ft5) ~?= [1 .. 4],
      "Remove Tail 6" ~: FT.toList (removeLast ft6) ~?= [1 .. 5],
      "Remove Tail 7" ~: FT.toList (removeLast ft7) ~?= [1 .. 6],
      "Remove Tail 8" ~: FT.toList (removeLast ft8) ~?= [1 .. 7],
      "Remove Tail 9" ~: FT.toList (removeLast ft9) ~?= [1 .. 8]
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
    [ "split empty index in bounds" ~: FT.split 0 ftEmpty ~?= (Nil, Nil),
      "split empty index out of bounds" ~: FT.split 10 ftEmpty ~?= (Nil, Nil),
      "split len 1, split at 0" ~: FT.split 0 ft1 ~?= (Nil, Unit 1),
      "split len 1, split at 1" ~: FT.split 1 ft1 ~?= (Unit 1, Nil),
      "split len 1, split out of bounds" ~: FT.split 2 ft1 ~?= (Unit 1, Nil),
      "split len 2, split at 0"
        ~: tupleMap (FT.split 0 ft2) FT.toList ~?= ([], [1, 2]),
      "split len 2, split at 1"
        ~: tupleMap (FT.split 1 ft2) FT.toList ~?= ([1], [2]),
      "split len 2, split at 2"
        ~: tupleMap (FT.split 2 ft2) FT.toList ~?= ([1, 2], []),
      "split len 2, split at 5"
        ~: tupleMap (FT.split 5 ft2) FT.toList ~?= ([1, 2], []),
      "split len 3, split at 0"
        ~: tupleMap (FT.split 0 ft3) FT.toList ~?= ([], [1, 2, 3]),
      "split len 3, split at 1"
        ~: tupleMap (FT.split 1 ft3) FT.toList ~?= ([1], [2, 3]),
      "split len 3, split at 2"
        ~: tupleMap (FT.split 2 ft3) FT.toList ~?= ([1, 2], [3]),
      "split len 4, split at 0"
        ~: tupleMap (FT.split 0 ft4) FT.toList ~?= ([], [1, 2, 3, 4]),
      "split len 4, split at 2"
        ~: tupleMap (FT.split 2 ft4) FT.toList ~?= ([1, 2], [3, 4]),
      "split len 4, split at 3"
        ~: tupleMap (FT.split 3 ft4) FT.toList ~?= ([1, 2, 3], [4]),
      "split len 5, split at 0"
        ~: tupleMap (FT.split 0 ft5) FT.toList ~?= ([], [1, 2, 3, 4, 5]),
      "split len 5, split at 3"
        ~: tupleMap (FT.split 3 ft5) FT.toList ~?= ([1, 2, 3], [4, 5]),
      "split len 6, split at 0"
        ~: tupleMap (FT.split 0 ft6) FT.toList ~?= ([], [1, 2, 3, 4, 5, 6]),
      "split len 6, split at 4"
        ~: tupleMap (FT.split 4 ft6) FT.toList ~?= ([1, 2, 3, 4], [5, 6]),
      "split len 7, split at 0"
        ~: tupleMap (FT.split 0 ft7) FT.toList ~?= ([], [1, 2, 3, 4, 5, 6, 7]),
      "split len 7, split at 5"
        ~: tupleMap (FT.split 5 ft7) FT.toList ~?= ([1, 2, 3, 4, 5], [6, 7]),
      "split len 8, split at 0"
        ~: tupleMap
          (FT.split 0 ft8)
          FT.toList
          ~?= ([], [1, 2, 3, 4, 5, 6, 7, 8]),
      "split len 8, split at 3"
        ~: tupleMap
          (FT.split 3 ft8)
          FT.toList
          ~?= ([1, 2, 3], [4, 5, 6, 7, 8]),
      "split len 8, split at 5"
        ~: tupleMap
          (FT.split 5 ft8)
          FT.toList
          ~?= ([1, 2, 3, 4, 5], [6, 7, 8]),
      "split len 9, split at 0"
        ~: tupleMap
          (FT.split 0 ft9)
          FT.toList
          ~?= ([], [1, 2, 3, 4, 5, 6, 7, 8, 9]),
      "split len 9, split at 1"
        ~: tupleMap
          (FT.split 1 ft9)
          FT.toList
          ~?= ([1], [2, 3, 4, 5, 6, 7, 8, 9]),
      "split len 9, split at 2"
        ~: tupleMap
          (FT.split 2 ft9)
          FT.toList
          ~?= ([1, 2], [3, 4, 5, 6, 7, 8, 9]),
      "split len 9, split at 3"
        ~: tupleMap
          (FT.split 3 ft9)
          FT.toList
          ~?= ([1, 2, 3], [4, 5, 6, 7, 8, 9]),
      "split len 9, split at 4"
        ~: tupleMap
          (FT.split 4 ft9)
          FT.toList
          ~?= ([1, 2, 3, 4], [5, 6, 7, 8, 9]),
      "split len 9, split at 5"
        ~: tupleMap
          (FT.split 5 ft9)
          FT.toList
          ~?= ([1, 2, 3, 4, 5], [6, 7, 8, 9]),
      "split len 9, split at 6"
        ~: tupleMap
          (FT.split 6 ft9)
          FT.toList
          ~?= ([1, 2, 3, 4, 5, 6], [7, 8, 9]),
      "split len 9, split at 7"
        ~: tupleMap
          (FT.split 7 ft9)
          FT.toList
          ~?= ([1, 2, 3, 4, 5, 6, 7], [8, 9]),
      "split len 9, split at 8"
        ~: tupleMap
          (FT.split 8 ft9)
          FT.toList
          ~?= ([1, 2, 3, 4, 5, 6, 7, 8], [9]),
      "split len 9, split at 9"
        ~: tupleMap
          (FT.split 9 ft9)
          FT.toList
          ~?= ([1, 2, 3, 4, 5, 6, 7, 8, 9], []),
      "split len 9, split at 10"
        ~: tupleMap
          (FT.split 10 ft9)
          FT.toList
          ~?= ([1, 2, 3, 4, 5, 6, 7, 8, 9], [])
    ]

tToList :: Test
tToList =
  TestList
    [ "toList empty" ~: FT.toList ftEmpty ~?= [],
      "toList 1" ~: FT.toList ft1 ~?= [1],
      "toList 2" ~: FT.toList ft2 ~?= [1, 2],
      "toList 3" ~: FT.toList ft3 ~?= [1, 2, 3],
      "toList 4" ~: FT.toList ft4 ~?= [1, 2, 3, 4],
      "toList 5" ~: FT.toList ft5 ~?= [1, 2, 3, 4, 5],
      "toList 6" ~: FT.toList ft6 ~?= [1, 2, 3, 4, 5, 6],
      "toList 7" ~: FT.toList ft7 ~?= [1, 2, 3, 4, 5, 6, 7],
      "toList 8" ~: FT.toList ft8 ~?= [1, 2, 3, 4, 5, 6, 7, 8],
      "toList 9" ~: FT.toList ft9 ~?= [1, 2, 3, 4, 5, 6, 7, 8, 9]
    ]

tFromList :: Test
tFromList =
  TestList
    [ "fromList empty" ~: ftEmpty ~?= FT.fromList [],
      "fromList 1" ~: ft1 ~?= FT.fromList [1],
      "fromList 2" ~: FT.toList ft2
        ~?= FT.toList (FT.fromList [1, 2]),
      "fromList 3" ~: FT.toList ft3
        ~?= FT.toList (FT.fromList [1, 2, 3]),
      "fromList 4" ~: FT.toList ft4
        ~?= FT.toList (FT.fromList [1, 2, 3, 4]),
      "fromList 5" ~: FT.toList ft5
        ~?= FT.toList (FT.fromList [1, 2, 3, 4, 5]),
      "fromList 6" ~: FT.toList ft6
        ~?= FT.toList (FT.fromList [1, 2, 3, 4, 5, 6]),
      "fromList 7" ~: FT.toList ft7
        ~?= FT.toList (FT.fromList [1, 2, 3, 4, 5, 6, 7]),
      "fromList 8" ~: FT.toList ft8
        ~?= FT.toList (FT.fromList [1, 2, 3, 4, 5, 6, 7, 8]),
      "fromList 9" ~: FT.toList ft9
        ~?= FT.toList (FT.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9])
    ]

tLength :: Test
tLength =
  TestList
    [ "Length empty" ~: FT.measure ftEmpty ~?= 0,
      "Length 1" ~: FT.measure ft1 ~?= 1,
      "Length 2" ~: FT.measure ft2 ~?= 2,
      "Length 3" ~: FT.measure ft3 ~?= 3,
      "Length 4" ~: FT.measure ft4 ~?= 4,
      "Length 5" ~: FT.measure ft5 ~?= 5,
      "Length 6" ~: FT.measure ft6 ~?= 6,
      "Length 7" ~: FT.measure ft7 ~?= 7,
      "Length 8" ~: FT.measure ft8 ~?= 8,
      "Length 9" ~: FT.measure ft9 ~?= 9
    ]

--------------- QuickCheck Properties ---------------

-- (1) PostCondition (Invariants) Properties

qcPostConditions :: IO ()
qcPostConditions = qc1 >> qc2 >> qc3 >> qc4 >> qc5 >> qc6 >> qc7

-- a. insertHead
prop_insertHeadHead :: FT.Measured a => Eq a => FT.FingerTree a -> a -> Bool
prop_insertHeadHead t x =
  let newTree = insertHead x t
   in case FT.head newTree of
        Nothing -> False
        Just v -> v == x

prop_insertHeadFirst :: FT.Measured a => Eq a => FT.FingerTree a -> a -> Bool
prop_insertHeadFirst t x =
  let newTree = insertHead x t
   in Prelude.head (FT.toList newTree) == x

-- b. insert tail
prop_insertTailTail :: FT.Measured a => Eq a => FT.FingerTree a -> a -> Bool
prop_insertTailTail t x =
  let newTree = insertTail x t
   in case FT.last newTree of
        Nothing -> False
        Just v -> v == x

prop_insertTailLast :: FT.Measured a => Eq a => FT.FingerTree a -> a -> Bool
prop_insertTailLast t x =
  let newTree = insertTail x t
   in Prelude.last (FT.toList newTree) == x

-- e. removeLast
prop_removeTailChanged :: FT.Measured a => Eq a => FT.FingerTree a -> Bool
prop_removeTailChanged t =
  let newTree = removeLast t
   in case t of
        Nil -> newTree == t
        _ -> newTree /= t

-- g. append
prop_append ::
  Eq a =>
  FT.Measured a =>
  FT.FingerTree a ->
  FT.FingerTree a ->
  Bool
prop_append t1 t2 =
  let t = append t1 t2
   in FT.head t1 == FT.head t
        && FT.last t2 == FT.last t
        && FT.toList t == FT.toList t1 ++ FT.toList t2

-- h. split
prop_split :: FT.Measured a => Eq a => Int -> FT.FingerTree a -> Bool
prop_split x t =
  let (t1, t2) = FT.split x t
   in (t1 == Nil || FT.head t == FT.head t1)
        && (t2 == Nil || FT.last t == FT.last t2)

qc1 :: IO ()
qc1 = quickCheck (prop_insertHeadHead :: FT.FingerTree Int -> Int -> Bool)

qc2 :: IO ()
qc2 = quickCheck (prop_insertHeadFirst :: FT.FingerTree Int -> Int -> Bool)

qc3 :: IO ()
qc3 = quickCheck (prop_insertTailTail :: FT.FingerTree Int -> Int -> Bool)

qc4 :: IO ()
qc4 = quickCheck (prop_insertTailLast :: FT.FingerTree Int -> Int -> Bool)

qc5 :: IO ()
qc5 = quickCheck (prop_removeTailChanged :: FT.FingerTree Int -> Bool)

qc6 :: IO ()
qc6 =
  quickCheck (prop_append :: FT.FingerTree Int -> FT.FingerTree Int -> Bool)

qc7 :: IO ()
qc7 = quickCheck (prop_split :: Int -> FT.FingerTree Int -> Bool)

-- (2) Metamorphic Properties (Bulk of the testing):

qcMetamorphic :: IO ()
qcMetamorphic = qc8 >> qc9 >> qc10 >> qc11 >> qc12 >> qc13

-- a.  insert at tail and then remove tail, should be previous tail
prop_insertRemoveTail :: FT.Measured a => Eq a => FT.FingerTree a -> a -> Bool
prop_insertRemoveTail t x =
  let t' = removeLast (insertTail x t)
   in FT.last t == FT.last t'

-- b. insert at tail twice and then remove tail, tails should be second tail
prop_insertTwice :: FT.Measured a => Eq a => FT.FingerTree a -> a -> a -> Bool
prop_insertTwice t x y =
  let t' = removeLast (insertTail y (insertTail x t))
   in FT.last t' == Just x

-- c. insert twice and then remove to check that they are still there
prop_insertRemoveTwice ::
  FT.Measured a => Eq a => FT.FingerTree a -> a -> a -> Bool
prop_insertRemoveTwice t x y =
  let t' = removeLast (removeLast (insertTail y (insertTail x t)))
   in FT.last t' == FT.last t

-- d. isempty -> insert -> should no longer be empty
prop_isEmptyInsert ::
  FT.Measured a => Eq a => FT.FingerTree a -> a -> Bool
prop_isEmptyInsert t x = not (isEmpty (insertHead x t))

-- e. insert head, insert tail and append trees
prop_insertAppend ::
  FT.Measured a => Eq a => FT.FingerTree a -> FT.FingerTree a -> a -> a -> Bool
prop_insertAppend t1 t2 x1 x2 =
  let t1' = insertHead x1 t1
   in let t2' = insertTail x2 t2
       in let t' = append t1' t2'
           in FT.head t' == FT.head t1'
                && FT.last t' == FT.last t2'
                && FT.toList t' == FT.toList t1' ++ FT.toList t2'

-- f. append after splitting
prop_SplitAppend :: FT.Measured a => Eq a => FT.FingerTree a -> Int -> Bool
prop_SplitAppend t x =
  let (t1, t2) = FT.split x t
   in FT.toList t == FT.toList (append t1 t2)

qc8 :: IO ()
qc8 = quickCheck (prop_insertRemoveTail :: FT.FingerTree Int -> Int -> Bool)

qc9 :: IO ()
qc9 = quickCheck (prop_insertTwice :: FT.FingerTree Int -> Int -> Int -> Bool)

qc10 :: IO ()
qc10 =
  quickCheck (prop_insertRemoveTwice :: FT.FingerTree Int -> Int -> Int -> Bool)

qc11 :: IO ()
qc11 = quickCheck (prop_isEmptyInsert :: FT.FingerTree Int -> Int -> Bool)

qc12 :: IO ()
qc12 =
  quickCheck
    ( prop_insertAppend ::
        FT.FingerTree Int -> FT.FingerTree Int -> Int -> Int -> Bool
    )

qc13 :: IO ()
qc13 = quickCheck (prop_SplitAppend :: FT.FingerTree Int -> Int -> Bool)

-- (3) Model-Based Poperties (could just use lists for now?)

qcModel :: IO ()
qcModel = qc14 >> qc15 >> qc16 >> qc17 >> qc18 >> qc19 >> qc20 >> qc21

prop_modelInsertHead :: FT.Measured a => Eq a => [a] -> Bool
prop_modelInsertHead l =
  let myTree = foldr insertHead Nil l
   in let modelTree = l
       in FT.toList myTree == modelTree

prop_modelInsertTail :: FT.Measured a => Eq a => [a] -> Bool
prop_modelInsertTail l =
  let myTree = foldl (flip insertTail) Nil l
   in let modelTree = l
       in FT.toList myTree == modelTree

prop_modelFromListHead :: FT.Measured a => Eq a => [a] -> Bool
prop_modelFromListHead l =
  let myTree = FT.fromList l
   in let modelTree = l
       in FT.head myTree == getFirst modelTree

prop_modelFromListTail :: FT.Measured a => Eq a => [a] -> Bool
prop_modelFromListTail l =
  let myTree = FT.fromList l
   in let modelTree = l
       in FT.last myTree == getLast modelTree

getFirst :: [a] -> Maybe a
getFirst (x : xs) = Just x
getFirst [] = Nothing

getLast :: [a] -> Maybe a
getLast [x] = Just x
getLast (x : xs) = getLast xs
getLast [] = Nothing

prop_modelFromListEveryElement :: FT.Measured a => Eq a => [a] -> Bool
prop_modelFromListEveryElement l =
  let myTree = FT.fromList l
   in let modelTree = l
       in FT.toList myTree == modelTree

prop_modelFromListEveryElementAppend :: 
 FT.Measured a => Eq a => [a] -> [a] -> Bool
prop_modelFromListEveryElementAppend l1 l2 =
  let myTree = append (FT.fromList l1) (FT.fromList l2)
   in let modelTree = l1 ++ l2
       in FT.toList myTree == modelTree

prop_modelSplit :: FT.Measured a => Eq a => Int -> [a] -> Bool
prop_modelSplit x l =
  let t = FT.fromList l
   in let (t1, t2) = FT.split x t
       in if x >= 0
            then FT.toList t1 == take x l && FT.toList t2 == drop x l
            else FT.toList t1 == drop x l && FT.toList t2 == take x l

prop_modelAppend :: FT.FingerTree Int -> FT.FingerTree Int -> Bool
prop_modelAppend t1 t2 = 
  FT.toList (FT.append t1 t2) == FT.toList t1 ++ FT.toList t2

qc14 :: IO ()
qc14 = quickCheck (prop_modelInsertHead :: [Int] -> Bool)

qc15 :: IO ()
qc15 = quickCheck (prop_modelInsertTail :: [Int] -> Bool)

qc16 :: IO ()
qc16 = quickCheck (prop_modelFromListHead :: [Int] -> Bool)

qc17 :: IO ()
qc17 = quickCheck (prop_modelFromListTail :: [Int] -> Bool)

qc18 :: IO ()
qc18 = quickCheck (prop_modelFromListEveryElement :: [Int] -> Bool)

qc19 :: IO ()
qc19 = 
  quickCheck (prop_modelFromListEveryElementAppend :: [Int] -> [Int] -> Bool)

qc20 :: IO ()
qc20 = quickCheck (prop_modelSplit :: Int -> [Int] -> Bool)

qc21 :: IO ()
qc21 = 
  quickCheck (prop_modelAppend :: 
  FT.FingerTree Int -> FT.FingerTree Int -> Bool
  )

qcFingerTree :: IO ()
qcFingerTree = putStrLn "I am in Missouri"

--------------- Sequence Tsts --------- ------

seqEmpty :: Sequence Int
seqEmpty = Sequence.empty

seq1 :: Sequence Int
seq1 = Sequence.singleton 1

seq2 :: Sequence Int
seq2 = seq1 Sequence.|> 2

seq3 :: Sequence Int
seq3 = seq2 Sequence.|> 3

seq4 :: Sequence Int
seq4 = seq3 Sequence.|> 4

seq5 :: Sequence Int
seq5 = seq4 Sequence.|> 5

tSeqUnit :: Test
tSeqUnit =
  TestList
    [ "Seq empty" ~: Sequence.toList seqEmpty ~?= [],
      "Seq 1" ~: Sequence.toList seq1 ~?= [1],
      "Seq 2" ~: Sequence.toList seq2 ~?= [1, 2],
      "Seq 3" ~: Sequence.toList seq3 ~?= [1, 2, 3],
      "Seq 4" ~: Sequence.toList seq4 ~?= [1, 2, 3, 4],
      "Seq 4" ~: Sequence.toList seq5 ~?= [1, 2, 3, 4, 5]
    ]

qcSequence :: IO ()
qcSequence =
  quickCheck prop_postConditionDeleteAt
    >> quickCheck prop_postConditionInsertHead
    >> quickCheck prop_postConditionInsertHead
    >> quickCheck prop_InsertAtLookUp
    >> quickCheck prop_deleteAtLookUp
    >> quickCheck prop_seqAppend
    >> quickCheck prop_seqToFromList

-- QuickCheck PostCondition Properties:
prop_postConditionDeleteAt :: Sequence Int -> Int -> Bool
prop_postConditionDeleteAt s i =
  let oldSize = Sequence.length s
   in let newSize = Sequence.length (Sequence.deleteAt i s)
       in if i >= 0 && i < Sequence.length s
            then (oldSize - 1) == newSize
            else oldSize == Sequence.length s

prop_postConditionInsertHead :: Sequence Int -> Int -> Bool
prop_postConditionInsertHead s x =
  let s1 = x Sequence.<| s
   in first s1 == Just x

prop_postConditionInsertTail :: Sequence Int -> Int -> Bool
prop_postConditionInsertTail s x =
  let s2 = s Sequence.|> x
   in first s2 == Just x

-- QuickCheck Metamorphic Properties:

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs =
  if idx < 0
    then xs
    else lft ++ rgt
  where
    (lft, rgt) = case splitAt idx xs of
      (y, []) -> (y, [])
      (y, _ : xs) -> (y, xs)

insertAt :: Int -> a -> [a] -> [a]
insertAt idx elt xs = lft ++ (elt : rgt)
  where
    (lft, rgt) = splitAt idx xs

prop_InsertAtLookUp :: Sequence Int -> Int -> Int -> Bool
prop_InsertAtLookUp s i x =
  let s1 = Sequence.insertAt i x s
   in if i == 0 || (i > 0 && i <= Sequence.length s)
        then Sequence.lookup i s1 == Just x
        else isNothing (Sequence.lookup i s1)

prop_deleteAtLookUp :: [Int] -> Int -> Bool
prop_deleteAtLookUp l i =
  let s = Sequence.fromList l
   in let s1 = Sequence.deleteAt i s
       in Sequence.toList s1 == Main.deleteAt i l

prop_seqAppend :: Sequence Int -> Sequence Int -> Bool
prop_seqAppend s1 s2 =
  let s = s1 Sequence.<> s2
   in Sequence.toList s == Sequence.toList s1 ++ Sequence.toList s2

prop_seqToFromList :: [Int] -> Bool
prop_seqToFromList l =
  let s = Sequence.fromList l
   in Sequence.toList s == l

----------- Priority Queue Tests -----------

pqEmpty :: PriorityQueue Int
pqEmpty = PQ.empty

pq1 :: PriorityQueue Int
pq1 = PQ.singleton 1

pq2 :: PriorityQueue Int
pq2 = enqueue 2 pq1

pq3 :: PriorityQueue Int
pq3 = enqueue 3 pq2

pq4 :: PriorityQueue Int
pq4 = enqueue 4 pq3

pq5 :: PriorityQueue Int
pq5 = enqueue 5 pq4

pqUnordered :: PriorityQueue Int
pqUnordered = PQ.fromList [1, 5, 2, 6, 3, 4, 5]

tPQUnit :: Test
tPQUnit =
  TestList
    [ "PQ empty" ~: fst (PQ.extractMax pqEmpty) ~?= Nothing,
      "PQ 1" ~: fst (PQ.extractMax pq1) ~?= Just 1,
      "PQ 2" ~: fst (PQ.extractMax pq2) ~?= Just 2,
      "PQ 3" ~: fst (PQ.extractMax pq3) ~?= Just 3,
      "PQ 4" ~: fst (PQ.extractMax pq4) ~?= Just 4,
      "PQ 5" ~: fst (PQ.extractMax pq5) ~?= Just 5
    ]

qcPQ :: IO ()
qcPQ =
  quickCheck prop_enqueue
    >> quickCheck prop_extractMax
    >> quickCheck prop_Size
    >> quickCheck prop_enqueuePeek
    >> quickCheck prop_enqueuePeek2
    >> quickCheck prop_enqueueSize
    >> quickCheck prop_extractMaxSize
    >> quickCheck prop_extractMaxPeekMax
    >> quickCheck prop_extractMaxEnqueuePeekMax

-- QuickCheck PostCondition/Model-based Properties:
prop_enqueue :: Ord a => PriorityQueue a -> a -> Bool
prop_enqueue pq x =
  let oldSize = size pq
   in oldSize + 1 == size (enqueue x pq)

prop_extractMax :: Ord a => PriorityQueue a -> Bool
prop_extractMax pq =
  let max = peekMax pq
   in let (max', _) = extractMax pq
       in max' == max

prop_Size :: Ord a => PriorityQueue a -> Bool
prop_Size pq =
  let size = PQ.size pq
   in size == Prelude.length (PQ.toList pq)

-- QuickCheck Metamorphic Properties:
prop_enqueuePeek :: PriorityQueue Int -> Bool
prop_enqueuePeek pq =
  let oldMax = peekMax pq
   in let newMax = case oldMax of
            Nothing -> 10
            Just v -> v + 1
       in let newPQ = enqueue newMax pq
           in peekMax newPQ == Just newMax

prop_enqueuePeek2 :: PriorityQueue Int -> Bool
prop_enqueuePeek2 pq =
  let oldMax = peekMax pq
   in let newMax = case oldMax of
            Nothing -> 10
            Just v -> v - 1
       in let newPQ = enqueue newMax pq
           in if isNothing oldMax 
             then peekMax newPQ == Just 10 
             else peekMax newPQ == oldMax

prop_enqueueSize :: Ord a => PriorityQueue a -> a -> Bool
prop_enqueueSize pq x =
  let oldSize = size pq
   in oldSize + 1 == size (enqueue x pq)

prop_extractMaxSize :: Ord a => PriorityQueue a -> Bool
prop_extractMaxSize pq =
  let oldSize = size pq
   in let (_, pq') = extractMax pq
       in (oldSize == 0) || (oldSize - 1 == size pq')

prop_extractMaxPeekMax :: Ord a => PriorityQueue a -> Bool
prop_extractMaxPeekMax pq =
  let oldMax = peekMax pq
   in case oldMax of
        Nothing -> peekMax pq == oldMax
        Just v ->
          let (_, pq') = extractMax pq
           in case peekMax pq' of
                Nothing -> True
                Just b -> v >= b

prop_extractMaxEnqueuePeekMax :: Ord a => PriorityQueue a -> Bool
prop_extractMaxEnqueuePeekMax pq =
  let (oldMax, pq') = extractMax pq
   in case oldMax of
        Nothing -> peekMax pq == oldMax
        Just v -> peekMax (enqueue v pq') == oldMax