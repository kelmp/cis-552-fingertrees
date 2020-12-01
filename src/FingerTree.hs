{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module FingerTree
  ( FingerTree (..),
    insertHead,
    insertTail,
    FingerTree.head,
    FingerTree.tail,
    isEmpty,
    append,
    split,
    toList,
    fromList,
    removeTail,
    FingerTree.concat,
    FingerTree.length,
    FingerTree.last,
  )
where

import Control.Applicative ()
import Control.Monad ()
import Data.Foldable ()
import Data.Functor ()
import Data.Monoid ()
import Data.Semigroup ()
import Data.Traversable ()
import Test.QuickCheck

data FingerTree v a
  = Nil
  | Unit a
  | More (Some a) (FingerTree v (Tuple a)) (Some a)
  deriving (Eq, Show)

data Some a
  = One a
  | Two a a
  | Three a a a
  deriving (Eq, Show)

data Tuple a
  = Pair a a
  | Triple a a a
  deriving (Eq, Show)

instance Monad FingerTree where
  return :: a -> FingerTree a
  return x = undefined

  (>>=) :: FingerTree a -> (a -> FingerTree b) -> FingerTree b
  t >>= f = undefined

instance Functor FingerTree where
  fmap :: (a -> b) -> FingerTree a -> FingerTree b
  fmap f t = undefined

instance Applicative FingerTree where
  pure :: a -> FingerTree a
  pure t = undefined

  (<*>) :: FingerTree (a -> b) -> FingerTree a -> FingerTree b
  t1 <*> t2 = undefined

instance Monoid (FingerTree a) where
  mempty :: FingerTree a
  mempty = Nil

instance Semigroup (FingerTree a) where
  (<>) = undefined

instance Foldable FingerTree where
  foldMap :: Monoid m => (a -> m) -> FingerTree a -> m
  foldMap f t = undefined

  foldr :: (a -> b -> b) -> b -> FingerTree a -> b
  foldr f b t = undefined

instance Traversable FingerTree where
  traverse :: Applicative z => (a -> z b) -> FingerTree a -> z (FingerTree b)
  traverse f t = undefined

------ Functions ------

insertHead :: a -> FingerTree a -> FingerTree a
insertHead a Nil = Unit a
insertHead a (Unit b) = More (One a) Nil (One b)
insertHead a (More (One b) ft r) = More (Two a b) ft r
insertHead a (More (Two b c) ft r) = More (Three a b c) ft r
insertHead a (More (Three b c d) ft r) =
  More (Two a b) (insertHead (Pair c d) ft) r

insertTail :: a -> FingerTree a -> FingerTree a
insertTail z Nil = Unit z
insertTail z (Unit a) = More (One a) Nil (One z)
insertTail z (More l ft (One a)) = More l ft (Two a z)
insertTail z (More l ft (Two a b)) = More l ft (Three a b z)
insertTail z (More l ft (Three a b c)) =
  More l (insertTail (Pair a b) ft) (Two c z)

head :: FingerTree a -> Maybe a
head Nil = Nothing
head (Unit x) = Just x
head (More (One x) _ _) = Just x
head (More (Two x _) _ _) = Just x
head (More (Three x _ _) _ _) = Just x

tail :: FingerTree a -> Maybe (FingerTree a)
tail (More (Three _ x y) ft r) = Just $ More (Two x y) ft r
tail (More (Two _ x) ft r) = Just $ More (One x) ft r
tail (More (One _) ft r) = case (ft, r) of
  (Nil, One x) -> Just $ Unit x
  (Nil, Two x y) -> Just $ More (One x) Nil (One y)
  (Nil, Three x y z) -> Just $ More (One x) Nil (Two y z)
  otherwise -> case FingerTree.head ft of
    Just (Pair x y) -> Just $ More (Two x y) (FingerTree.tail ft) r

last :: FingerTree a -> Maybe a
last Nil = Nothing
last (Unit x) = Just x
last (More _ _ (One x)) = Just x
last (More _ _ (Two _ x)) = Just x
last (More _ _ (Three _ _ x)) = Just x

removeTail :: FingerTree a -> FingerTree a
removeTail = undefined

isEmpty :: FingerTree a -> Bool
isEmpty t = undefined

append :: FingerTree a -> FingerTree a -> FingerTree a
append t1 t2 = glue t1 [] t2

glue :: FingerTree a -> [a] -> FingerTree a -> FingerTree a
glue Nil l t2 = foldr insertHead t2 l
glue t1 l Nil = foldl (flip insertTail) t1 l
glue (Unit x) l t2 = foldr insertHead t2 (x : l)
glue t1 l (Unit y) = foldl (flip insertTail) t1 (l ++ [y])
glue (More x1 t1 y1) l (More x2 t2 y2) =
  More x1 (glue t1 (listToTuples (someToList y1 ++ l ++ someToList x2)) t2) y2

someToList :: Some a -> [a]
someToList (One x) = [x]
someToList (Two x y) = [x, y]

listToTuples :: [a] -> [Tuple a]
listToTuples [] = []
listToTuples [x, y] = [Pair x y]
listToTuples [x, y, z, w] = [Pair x y, Pair z w]
listToTuples (x : y : z : xs) = Triple x y z : listToTuples xs

split :: Measured v a => (v -> Bool) -> FingerTree v a -> (FingerTree v a, FingerTree v a)
split t = undefined

concat :: [FingerTree a] -> FingerTree a
concat l = undefined

toList :: FingerTree a -> [a]
toList t = undefined

length :: FingerTree a -> Int
length = undefined

-- ft8 :: FingerTree Int
-- ft8 = More (One 1) (More (One (Pair 2 3)) Nil (One (Pair 4 5))) (Three 6 7 8)

fromList :: [a] -> FingerTree a
fromList = foldr insertTail Nil

instance (Show a, Arbitrary a) => Arbitrary (FingerTree a) where
  arbitrary = undefined
  shrink = undefined
