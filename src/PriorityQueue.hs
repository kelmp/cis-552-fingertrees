{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module PriorityQueue (
  PriorityQueue,
  extractMax,
  PriorityQueue.fmap',
  PriorityQueue.empty,
  singleton,
  size,
  peekMax,
  enqueue,
  PriorityQueue.fromList
) where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Functor
import Data.Monoid
import Data.Semigroup
import Data.Traversable
import FingerTree2 as FT

newtype PriorityQueue a = PQ (FingerTree (Prio a) (Elem a))
  deriving (Show, Eq)

newtype Elem a = Elem { getElem :: a }
  deriving (Show, Eq)

instance Ord a => Measured (Prio a) (Elem a) where
  measure (Elem x) = Prio x

data Prio a
  = MinInfty
  | Prio a
  deriving (Eq, Ord, Show)

instance (Ord a) => Monoid (Prio a) where
  mempty = MinInfty
  mappend = (<>)

instance (Ord a) => Semigroup (Prio a) where
  MinInfty <> y = y
  x <> MinInfty = x
  x <> y = max x y

extractMax :: (Ord a) => PriorityQueue a -> (Maybe a, PriorityQueue a)
extractMax pq@(PQ Nil) = (Nothing, pq)
extractMax (PQ ft) = (Just x, PQ (append l r))
  where Split l (Elem x) r = splitTree (measure ft <=) mempty ft

fmap' :: (Ord a, Ord b) => (a -> b) -> PriorityQueue a -> PriorityQueue b
fmap' f (PQ t) = PQ (FT.fmap' (\(Elem x) -> Elem (f x)) t)

-- Old implementation functions, translated to FingerTree2
empty :: PriorityQueue a
empty = PQ Nil

singleton :: a -> PriorityQueue a
singleton = PQ . Unit . Elem

size :: PriorityQueue a -> Int
size (PQ t) = length t

peekMax :: Ord a => PriorityQueue a -> Maybe a
peekMax = fst . extractMax

enqueue :: Ord a => a -> PriorityQueue a -> PriorityQueue a
enqueue x (PQ t) = PQ $ insertHead (Elem x) t

fromList :: Ord a => [a] -> PriorityQueue a
fromList = foldr enqueue PriorityQueue.empty

-- Ord constraint required, which violates type signature of fmap
-- instance Functor PriorityQueue where
--   fmap :: (a -> b) -> PriorityQueue a -> PriorityQueue b
--   fmap f (PQ t) = PQ (FT.fmap' (\(Elem x) -> Elem (f x)) t)

-- Functor needed for Applicative
-- instance Applicative PriorityQueue where
--   pure :: a -> PriorityQueue a
--   pure x = PQ $ Unit $ Elem x

--   (<*>) :: PriorityQueue (a -> b) -> PriorityQueue a -> PriorityQueue b
--   (<*>) = undefined
  -- t1 <*> t2 = FingerTree . (<*>)

-- toList :: (Ord a, Measured c (FingerTree c a)) => PriorityQueue a -> [a]
-- toList (PQ ft) = FT.toList $ FT.fmap' getElem ft

-- union :: Ord a => PriorityQueue a -> PriorityQueue a -> PriorityQueue a
-- union = flip enqueue


-- OLD IMPL BELOW - from pre-FingerTree2 times

-- Invariants:
-- a must be instance of Ord
-- head is min, last is max (smallest priority to greatest)

-- data PriorityQueue a = PriorityQueue {ft :: FingerTree a, kind :: MinOrMax}

-- data MinOrMax = MinQueue | MaxQueue

-- Instances --

-- instance Monad PriorityQueue where
--   return :: a -> PriorityQueue a
--   return = pure

--   (>>=) :: PriorityQueue a -> (a -> PriorityQueue b) -> PriorityQueue b
--   t >>= f = FingerTree . (>>=)

-- foldl add empty t
--  where
--   add x y = x >< f y

-- instance Functor PriorityQueue where
--   fmap :: (a -> b) -> PriorityQueue a -> PriorityQueue b
--   fmap f (PQ t) = PQ (FT.fmap' f t)

-- instance Applicative PriorityQueue where
--   pure :: a -> PriorityQueue a
--   pure t = undefined -- TODO

--   (<*>) :: PriorityQueue (a -> b) -> PriorityQueue a -> PriorityQueue b
--   t1 <*> t2 = FingerTree . (<*>)

-- instance Measured a => Monoid (PriorityQueue a) where
--   mempty :: PriorityQueue a
--   mempty = PQ Nil

-- instance Measured a => Semigroup (PriorityQueue a) where
--   (PQ t1) <> (PQ t2) = PQ ((<>) t1 t2)

-- instance Foldable PriorityQueue where
--   foldMap :: Monoid m => (a -> m) -> PriorityQueue a -> m
--   foldMap f (PQ t) = PQ (foldMap f t)

-- foldr :: (a -> b -> b) -> b -> PriorityQueue a -> b
-- foldr f b t = FT.foldr f b t

-- instance Traversable PriorityQueue where
--   traverse :: Applicative z => (a -> z b) -> PriorityQueue a -> z (PriorityQueue b)
--   traverse f t = FT.traverse f t

------ Functions ------

-- empty :: PriorityQueue a
-- empty = PQ Nil

-- singleton :: Measured a => Ord a => a -> PriorityQueue a
-- singleton = PQ . Unit

-- size :: Measured a => PriorityQueue a -> Int
-- size (PQ t) = measure t

-- peekMax :: Measured a => PriorityQueue a -> Maybe a
-- peekMax (PQ t) = FT.last t

-- peekMin :: Measured a => PriorityQueue a -> Maybe a
-- peekMin (PQ t) = FT.head t

-- deleteMax :: Measured a => PriorityQueue a -> PriorityQueue a
-- deleteMax (PQ t) = PQ (removeLast t)

-- deleteMin :: Measured a => PriorityQueue a -> PriorityQueue a
-- deleteMin (PQ t) = PQ $ FT.tail t

-- enqueue :: Ord a => a -> PriorityQueue a -> PriorityQueue a
-- enqueue x (PQ t) =
--   let (t1, t2) = split (<= x) t
--    in PQ (append (insertHead x t2) t2)

-- union :: Ord a => PriorityQueue a -> PriorityQueue a -> PriorityQueue a
-- union = flip enqueue

-- insert all the elements from second queue into the first one