{-# LANGUAGE ScopedTypeVariables #-}

import FingerTree ( fromList, insertHead, FingerTree (..) )
import Test.HUnit
import Test.QuickCheck
import Data.Maybe as Maybe

main :: IO ()
main = testAll

testAll :: IO ()
testAll = do
  allHUnit
  allQuickCheck
  putStrLn ""

allHUnit :: IO ()
allHUnit = do
  _ <- runTestTT
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

<<<<<<< HEAD
-- FingerTree Tests --
=======
ftEmpty :: FingerTree Int
ftEmpty = Nil

ftOne :: FingerTree Int
ftOne = fromList [1]

ftTen :: FingerTree Int
ftTen = fromList [1..10]
>>>>>>> 491738951be6fd251a7605798be094d1a616ab64

tConstruct :: Test
tConstruct =
  TestList
    [ "Empty tree" ~: ftEmpty ~?= ftEmpty,
      "Unit tree" ~: insertHead 1 mempty ~?= ftOne,
      "Larger tree" ~: undefined
    ]

tInsertHead :: Test
<<<<<<< HEAD
tInsertHead = undefined

-- Priority Queue Tests --

-- Sequence Tests --
=======
tInsertHead =
  TestList
  [ "Insert head empty" ~: undefined,
    "Insert head simple" ~: undefined,
    "Insert head complex" ~: undefined]

tInsertTail :: Test
tInsertTail =
  TestList
  [ "Insert tail empty" ~: undefined,
    "Insert tail simple" ~: undefined,
    "Insert tail complex" ~: undefined]

tHead :: Test
tHead =
  TestList
  [ "Head empty" ~: undefined,
    "Head simple" ~: undefined,
    "Head complex" ~: undefined]

tTail :: Test
tTail =
  TestList
  [ "Tail empty" ~: undefined,
    "Tail simple" ~: undefined,
    "Tail complex" ~: undefined]

tIsEmpty :: Test
tIsEmpty =
  TestList
  [ "Empty is empty" ~: undefined,
    "Nonempty is not empty" ~: undefined]

tConcat :: Test
tConcat =
  TestList
  [ "Concat two empty" ~: undefined,
    "Concat first empty" ~: undefined,
    "Concat second empty" ~: undefined,
    "Concat both simple" ~: undefined,
    "Concat both complex" ~: undefined]

tSplit :: Test
tSplit =
  TestList
  [ "Split all left" ~: undefined,
    "Split all right" ~: undefined,
    "Split middle" ~: undefined]

tMap :: Test
tMap =
  TestList
  [ "Map identity" ~: undefined,
    "Map to same type" ~: undefined,
    "Map to diff type" ~: undefined]

tToList :: Test
tToList =
  TestList
  [ "toList empty" ~: undefined,
    "toList simple" ~: undefined,
    "toList complex" ~: undefined]

prop_length :: FingerTree Int -> Bool
prop_length ft = Maybe.isJust (count ft) where
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

qc7 :: IO()
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
qcFingerTree = qc1 >> qc2 >> qc3 >> qc4 >> qc5 >> qc6 >>
  qc7 >> qc8 >> qc9 >> qc10 >> qc11 >> qc12
>>>>>>> 491738951be6fd251a7605798be094d1a616ab64
