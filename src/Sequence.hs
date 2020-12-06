{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Sequence where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Functor
import Data.Monoid
import Data.Semigroup
import Data.Traversable
import FingerTree

newtype Sequence a = FingerTree a

-- Instances --

instance Monad Sequence where
  return :: a -> Sequence a
  return = pure

  (>>=) :: Sequence a -> (a -> Sequence b) -> Sequence b
  t >>= f = FingerTree . (>>=)

-- foldl add empty t
--  where
--   add x y = x >< f y

instance Functor Sequence where
  fmap :: (a -> b) -> Sequence a -> Sequence b
  fmap f t = FingerTree.fmap

instance Applicative Sequence where
  pure :: a -> Sequence a
  pure t = singleton

  (<*>) :: Sequence (a -> b) -> Sequence a -> Sequence b
  t1 <*> t2 = FingerTree . (<*>)

instance Monoid (Sequence a) where
  mempty :: Sequence a
  mempty = empty

instance Semigroup (Sequence a) where
  (<>) = FingerTree . (<>)

instance Foldable Sequence where
  foldMap :: Monoid m => (a -> m) -> Sequence a -> m
  foldMap f t = FingerTree.foldMap

  foldr :: (a -> b -> b) -> b -> Sequence a -> b
  foldr f b t = FingerTree.foldr f b t

instance Traversable Sequence where
  traverse :: Applicative z => (a -> z b) -> Sequence a -> z (Sequence b)
  traverse f t = FingerTree.traverse f t

------ Functions ------

empty :: Sequence a
empty = Nil

singleton :: a -> Sequence a
singleton = Unit

(<|) :: a -> Sequence a -> Sequence a
(<|) = insertHead

(|>) :: Sequence a -> a -> Sequence a
(|>) = inserTail

(><) :: Sequence a -> Sequence a -> Sequence a
(><) = append

first :: Seq a -> Maybe a
first = FingerTree.head

last :: Seq a -> Maybe a
last = FingerTree.last

deleteLast :: Seq a -> Seq a
deleteLast = removeTail

deleteAt :: Int -> Seq a -> Seq a
deleteAt i seq =
  let (t1, t2) = split i seq
   in case t2 of
        Nil -> seq
        _ -> append t1 (removeHead t2)

insertAt :: Int -> a -> Seq a -> Seq a

deleteAt i x seq =
  let (t1, t2) = split i seq
   in case t2 of
        Nil -> seq
        _ -> append t1 (insertHead t2 x)

fromList :: [a] -> Sequence a
fromList = FingerTree.fromList

null :: Sequence a -> Bool
null = (== Nil)

length :: Sequnece a -> Int
length = FingerTree.length

lookup :: Int -> Sequence a -> Maybe a
lookup x seq =
  let (_, t2) = split x seq
   in if t2 /= Nil then Just FingerTree.head t2 else Nothing