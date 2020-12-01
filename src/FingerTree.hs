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

data FingerTree a
  = Nil
  | Unit a
  | More (Some a) (FingerTree (Tuple a)) (Some a)
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
head t = undefined

tail :: FingerTree a -> Maybe a
tail t = undefined

removeTail :: FingerTree a -> FingerTree a
removeTail = undefined

isEmpty :: FingerTree a -> Bool
isEmpty t = undefined

append :: FingerTree a -> FingerTree a -> FingerTree a
append t1 t2 = undefined

split :: FingerTree a -> (FingerTree a, FingerTree a)
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
