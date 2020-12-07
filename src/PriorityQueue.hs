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
import FingerTree as FT

newtype PriorityQueue a = Ord a => FingerTree a

-- Invariants:
-- a must be instance of Ord
-- head is min, last is max (smallest priority to greatest)

-- data PriorityQueue a = PriorityQueue {ft :: FingerTree a, kind :: MinOrMax}

-- data MinOrMax = MinQueue | MaxQueue

-- Instances --

instance Monad PriorityQueue where
  return :: a -> PriorityQueue a
  return = pure

  (>>=) :: PriorityQueue a -> (a -> PriorityQueue b) -> PriorityQueue b
  t >>= f = FingerTree . (>>=)

-- foldl add empty t
--  where
--   add x y = x >< f y

instance Functor PriorityQueue where
  fmap :: (a -> b) -> PriorityQueue a -> PriorityQueue b
  fmap f t = FT.fmap

instance Applicative PriorityQueue where
  pure :: a -> PriorityQueue a
  pure t = undefined -- TODO

  (<*>) :: PriorityQueue (a -> b) -> PriorityQueue a -> PriorityQueue b
  t1 <*> t2 = FingerTree . (<*>)

instance Monoid (PriorityQueue a) where
  mempty :: PriorityQueue a
  mempty = empty

instance Semigroup (PriorityQueue a) where
  (<>) = FingerTree . (<>)

instance Foldable PriorityQueue where
  foldMap :: Monoid m => (a -> m) -> PriorityQueue a -> m
  foldMap f t = FT.foldMap

  foldr :: (a -> b -> b) -> b -> PriorityQueue a -> b
  foldr f b t = FT.foldr f b t

instance Traversable PriorityQueue where
  traverse :: Applicative z => (a -> z b) -> PriorityQueue a -> z (PriorityQueue b)
  traverse f t = FT.traverse f t

------ Functions ------

empty :: PriorityQueue a
empty = Nil

singleton :: Ord a => a -> PriorityQueue a
singleton = Unit

size :: PriorityQueue a -> Int
size = length

peekMax :: PriorityQueue a -> Maybe a
peekMax = FT.last

peekMin :: PriorityQueue a -> Maybe a
peekMin = FT.head

deleteMax :: PriorityQueue a -> PriorityQueue a
deleteMax = removeTail

deleteMin :: PriorityQueue a -> PriorityQueue a
deleteMin = removeHead

enqueue :: Ord a => a -> PriorityQueue a -> PriorityQueue a
enqueue x q =
  let (t1, t2) = split (<= x) q
   in append (insertHead t2 x) t2

union :: Ord a => PriorityQueue a -> PriorityQueue a -> PriorityQueue a
union = flip enqueue

-- insert all the elements from second queue into the first one