{-# LANGUAGE ScopedTypeVariables #-}

import Data.Maybe as Maybe
import FingerTree (FingerTree (..), fromList, insertHead)
import Test.HUnit
import Test.QuickCheck

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
            tConstruct,
            tInsertHead,
            tInsertTail,
            tHead,
            tTail,
            tIsEmpty,
            tConcat,
            tSplit,
            tMap
          ]
      )
  putStrLn ""

allQuickCheck :: IO ()
allQuickCheck = qcFingerTree

quickCheckN :: Test.QuickCheck.Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith $ stdArgs {maxSuccess = n, maxSize = 100}

verboseCheckN :: Test.QuickCheck.Testable prop => Int -> prop -> IO ()
verboseCheckN n = verboseCheckWith $ stdArgs {maxSuccess = n, maxSize = 100}

-- TODO:
-- (1) fill in unit tests
-- (2) add quickcheck properties for all the functions
-- (3) add quickcheck properties for remaining type-classes that FingerTrees implements
-- (3) create a quickcheck property that checks if its a valid Finger Tree (not sure how exactly this will be defines)
--     and use that to replace AVL_prop preoprty
-- (4) add unit tests and quickcheck properties for priority queues and sequences

ftEmpty :: FingerTree Int
ftEmpty = Nil

ft1 :: FingerTree Int
ft1 = Unit 1

ft2 :: FingerTree Int
ft2 = More (One 1) Nil (One 2)

ft3 :: FingerTree Int
ft3 = More (One 1) Nil (Two 2 3)

ft4 :: FingerTree Int
ft4 = More (One 1) Nil (Three 2 3 4)

ft5 :: FingerTree Int
ft5 = More (One 1) (Unit (Pair 2 3)) (Two 4 5)

ft6 :: FingerTree Int
ft6 = More (One 1) (Unit (Pair 2 3)) (Three 4 5 6)

ft7 :: FingerTree Int
ft7 = More (One 1) (More (One (Pair 2 3)) Nil (One (Pair 4 5))) (Two 6 7)

ft8 :: FingerTree Int
ft8 = More (One 1) (More (One (Pair 2 3)) Nil (One (Pair 4 5))) (Three 6 7 8)

ft9 :: FingerTree Int
ft9 =
  More
    (One 1)
    (More (One (Pair 2 3)) Nil (Two (Pair 4 5) (Pair 6 7)))
    (Two 8 9)

tInsertHead :: Test
tInsertHead =
  TestList
    [ "Insert head 0 -> 1" ~: insertHead 1 Nil ~?= Unit 1,
      "Insert head 1 -> 2" ~: insertHead 2 (Unit 1)
        ~?= More (One 2) Nil (One 1),
      "Insert head 4 -> 5" ~: insertHead 5 (More (Three 4 3 2) Nil (One 1))
        ~?= More (Two 5 4) (Unit (Pair 3 2)) (One 1),
      "Insert head 6 -> 7"
        ~: insertHead 7 (More (Three 6 5 4) (Unit (Pair 3 2)) (One 1))
        ~?= More (Two 7 6) (More (One (Pair 5 4)) Nil (One (Pair 3 2))) (One 1),
      "Insert head 8 -> 9"
        ~: insertHead
          9
          ( More
              (Three 8 7 6)
              (More (One (Pair 5 4)) Nil (One (Pair 3 2)))
              (One 1)
          )
        ~?= More
          (Two 9 8)
          More
          ((Two (Pair 7 6) (Pair 5 4)) Nil (One (Pair 3 2)))
          (One 1)
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
    [ "Head empty" ~: head ftEmpty ~?= Nothing,
      "Head unit" ~: head ft1 ~?= Just 1,
      "Head more (one ...)" ~: head ft9 ~?= Just 1,
      "Head more (two ....)" ~: head (insertHead 0 ft9) ~?= Just 0,
      "Head more (three ....)" ~: head (insertHead (-1) (insertHead 0 ft9)) ~?= Just (-1)
    ]

tTail :: Test
tTail =
  TestList
    [ "Tail empty" ~: tail ftEmpty ~?= Nothing,
      "Tail 1" ~: tail ft1 ~?= Just 1,
      "Tail 2" ~: tail ft2 ~?= Just 2,
      "Tail 3" ~: tail ft3 ~?= Just 3,
      "Tail 4" ~: tail ft4 ~?= Just 4,
      "Tail 5" ~: tail ft5 ~?= Just 5,
      "Tail 6" ~: tail ft6 ~?= Just 6,
      "Tail 7" ~: tail ft7 ~?= Just 7,
      "Tail 8" ~: tail ft8 ~?= Just 8,
      "Tail 9" ~: tail ft9 ~?= Just 9
    ]

tRemoveTail :: Test
tRemoveTail =
  TestList
    [ "Remove Tail Empty" ~: removeTail ftEmpty ~?= ftEmpty,
      "Remove Tail 1" ~: removeTail ft1 ~?= ftEmpty,
      "Remove Tail 2" ~: removeTail ft2 ~?= ft1,
      "Remove Tail 3" ~: removeTail ft3 ~?= ft2,
      "Remove Tail 4" ~: removeTail ft4 ~?= ft3,
      "Remove Tail 5" ~: removeTail ft5 ~?= ft4,
      "Remove Tail 6" ~: removeTail ft6 ~?= ft5,
      "Remove Tail 7" ~: removeTail ft7 ~?= ft6,
      "Remove Tail 8" ~: removeTail ft8 ~?= ft7,
      "Remove Tail 9" ~: removeTail ft9 ~?= ft8
    ]

tIsEmpty :: Test
tIsEmpty =
  TestList
    [ "Empty is empty" ~: isEmpty ftEmpty ~?= True,
      "Nonempty is not empty"
        ~: all
          (isEmpty <$> [ft1, ft2, ft3, ft4, ft5, ft6, ft7, ft8, ft9])
        ~?= True
    ]

tConcat :: Test
tConcat =
  TestList
    [ "Concat two empty" ~: undefined,
      "Concat first empty" ~: undefined,
      "Concat second empty" ~: undefined,
      "Concat both simple" ~: undefined,
      "Concat both complex" ~: undefined
    ]

tSplit :: Test
tSplit =
  TestList
    [ "Split all left" ~: undefined,
      "Split all right" ~: undefined,
      "Split middle" ~: undefined
    ]

tMap :: Test
tMap =
  TestList
    [ "Map identity" ~: undefined,
      "Map to same type" ~: undefined,
      "Map to diff type" ~: undefined
    ]

tToList :: Test
tToList =
  TestList
    [ "toList empty" ~: undefined,
      "toList simple" ~: undefined,
      "toList complex" ~: undefined
    ]

prop_length :: FingerTree Int -> Bool
prop_length ft = Maybe.isJust (count ft)
  where
    count Nil = Just 0
    count (Unit _) = Just 1
    count (More l ft r) = undefined -- count each and add

prop_append :: FingerTree Int -> FingerTree Int -> Bool
prop_append t1 t2 = toList (t1 <> t2) == toList t1 ++ toList t2

prop_fMapId :: (Eq (f a), Functor f) => f a -> Bool
prop_fMapId t = fmap id t == id t

prop_FMapComp :: (Eq (f c), Functor f) => Fun b c -> Fun a b -> f a -> Bool
prop_FMapComp (Fun _ f) (Fun _ g) x =
  fmap (f . g) x == (fmap f . fmap g) x

prop_LeftUnit :: (Eq (m b), Monad m) => a -> Fun a (m b) -> Bool
prop_LeftUnit x (Fun _ f) =
  (return x >>= f) == f x

prop_RightUnit :: (Eq (m b), Monad m) => m b -> Bool
prop_RightUnit m =
  (m >>= return) == m

prop_Assoc ::
  (Eq (m c), Monad m) =>
  m a ->
  Fun a (m b) ->
  Fun b (m c) ->
  Bool
prop_Assoc m (Fun _ f) (Fun _ g) =
  ((m >>= f) >>= g) == (m >>= f x >=> g)

prop_FunctorMonad :: (Eq (m b), Monad m) => m a -> Fun a b -> Bool
prop_FunctorMonad x (Fun _ f) = fmap f x == (f <$> x)

qc1 :: IO ()
qc1 = quickCheck (prop_FMapId :: FingerTree Int -> Bool)

qc2 :: IO ()
qc2 =
  quickCheck
    (prop_FMapComp :: Fun Int Int -> Fun Int Int -> FingerTree Int -> Bool)

qc3 :: IO ()
qc3 = quickCheck (prop_LeftUnit :: Int -> Fun Int (FingerTree Int) -> Bool)

qc4 :: IO ()
qc4 = quickCheck (prop_RightUnit :: FingerTree Int -> Bool)

-- warning, this one is slower than the rest.
-- It takes 10-15 seconds on my machine.
qc5 :: IO ()
qc5 =
  quickCheck
    (prop_Assoc :: FingerTree Int -> Fun Int (FingerTree Int) -> Fun Int (FingerTree Int) -> Bool)

qc6 :: IO ()
qc6 = quickCheck (prop_FunctorMonad :: FingerTree Int -> Fun Int (FingerTree Int) -> Bool)

qc7 :: IO ()
qc7 = quickCheck (prop_qc7 :: FingerTree Int -> Fun Int Int -> Bool)

prop_qc7 :: (Eq b) => FingerTree a -> Fun a b -> Bool
prop_qc7 s (Fun _ f) = toList (fmap f s) == fmap f (toList s)

qc8 :: IO ()
qc8 = quickCheck (prop_qc8 :: Int -> Bool)

prop_qc8 :: (Eq a) => a -> Bool
prop_qc8 (x :: a) = toList (return x :: FingerTree a) == return x

qc9 :: IO ()
qc9 = quickCheck (prop_qc9 :: FingerTree Int -> Fun Int (FingerTree Int) -> Bool)

prop_qc9 :: (Eq b) => FingerTree a -> Fun a (FingerTree b) -> Bool
prop_qc9 m (Fun _ k) = toList (m >>= k) == (toList m >>= (toList . k))

qc10 :: IO ()
qc10 = quickCheck prop_FingerTree_functor
  where
    prop_FingerTree_functor :: Fun Int Int -> FingerTree Int -> Property
    prop_FingerTree_functor (Fun _ f) x = prop_AVL (fmap f x)

qc11 :: IO ()
qc11 = quickCheck prop_FingerTree_return
  where
    prop_FingerTree_return :: Int -> Property
    prop_FingerTree_return x = prop_AVL (return x)

qc12 :: IO ()
qc12 = quickCheck prop_FingerTree_bind
  where
    prop_FingerTree_bind :: FingerTree Int -> Fun Int (FingerTree Int) -> Property
    prop_FingerTree_bind x (Fun _ k) = prop_AVL (x >>= k)

qcFingerTree :: IO ()
qcFingerTree =
  qc1 >> qc2 >> qc3 >> qc4 >> qc5 >> qc6
    >> qc7
    >> qc8
    >> qc9
    >> qc10
    >> qc11
    >> qc12
