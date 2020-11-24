{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module PriorityQueue where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Functor
import Data.Monoid
import Data.Semigroup
import Data.Traversable
import FingerTree

data PriorityQueue a = PriorityQueue {ft :: FingerTree a, kind :: MinOrMax}

data MinOrMax = MinQueue | MaxQueue

-- Instances --

instance Monad PriorityQueue where
  return :: a -> PriorityQueue a
  return x = undefined

  (>>=) :: PriorityQueue a -> (a -> PriorityQueue b) -> PriorityQueue b
  t >>= f = undefined

instance Functor PriorityQueue where
  fmap :: (a -> b) -> PriorityQueue a -> PriorityQueue b
  fmap f t = undefined

instance Applicative PriorityQueue where
  pure :: a -> PriorityQueue a
  pure t = undefined

  (<*>) :: PriorityQueue (a -> b) -> PriorityQueue a -> PriorityQueue b
  t1 <*> t2 = undefined

instance Monoid (PriorityQueue a) where
  mempty :: PriorityQueue a
  mempty = undefined

instance Semigroup (PriorityQueue a) where
  (<>) = undefined

instance Foldable PriorityQueue where
  foldMap :: Monoid m => (a -> m) -> PriorityQueue a -> m
  foldMap f t = undefined

  foldr :: (a -> b -> b) -> b -> PriorityQueue a -> b
  foldr f b t = undefined

instance Traversable PriorityQueue where
  traverse :: Applicative z => (a -> z b) -> PriorityQueue a -> z (PriorityQueue b)
  traverse f t = undefined

------ Functions ------

emptyMaxQueue :: PriorityQueue a
emptyMaxQueue = undefined

emptyMinQueue :: PriorityQueue a
emptyMinQueue = undefined

nullMinQueue :: PriorityQueue a
nullMinQueue = undefined

nullMaxQueue :: PriorityQueue a
nullMaxQueue = undefined

size :: PriorityQueue a -> Int
size = undefined

findMax :: PriorityQueue a -> Maybe a
findMax = undefined

findMin :: PriorityQueue a -> Maybe a
findMin = undefined

getMax :: PriorityQueue a -> Maybe a
getMax = undefined

getMin :: PriorityQueue a -> Maybe a
getMin = undefined

deleteMax :: PriorityQueue a -> PriorityQueue a
deleteMax = undefined

deleteMin :: PriorityQueue a -> PriorityQueue a
deleteMin = undefined

singletonMax :: a -> PriorityQueue a
singletonMax = undefined

singletonMin :: a -> PriorityQueue a
singletonMin = undefined

insert :: Ord a => a -> PriorityQueue a -> PriorityQueue a -> PriorityQueue a
insert = undefined

union :: Ord a => PriorityQueue a -> PriorityQueue a -> PriorityQueue a
union = undefined