{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Sequence where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Functor ()
import Data.Monoid
import Data.Semigroup
import Data.Traversable
import FingerTree
import Test.QuickCheck

-- Sequence type is comparable to Haskell's Data.Sequence
newtype Sequence a = Seq (FingerTree a)
  deriving (Eq, Show)

instance Measured a => Monoid (Sequence a) where
  mempty :: Sequence a
  mempty = Seq Nil

instance Measured a => Semigroup (Sequence a) where
  (Seq t1) <> (Seq t2) = Seq (FingerTree.append t1 t2)

empty :: Sequence a
empty = Seq Nil

singleton :: Measured a => a -> Sequence a
singleton x = Seq $ Unit x

-- Insert new element at beg. of sequence
(<|) :: Measured a => a -> Sequence a -> Sequence a
(<|) x (Seq t) = Seq (insertHead x t)

-- Insert new element at end of sequence
(|>) :: Measured a => Sequence a -> a -> Sequence a
(|>) (Seq t) x = Seq (insertTail x t)

-- Append two sequences together
(<>) :: Measured a => Sequence a -> Sequence a -> Sequence a
(<>) (Seq t1) (Seq t2) = Seq (append t1 t2)

first :: Sequence a -> Maybe a
first (Seq t1) = FingerTree.head t1

last :: Sequence a -> Maybe a
last (Seq t1) = FingerTree.last t1

deleteAt :: Measured a => Int -> Sequence a -> Sequence a
deleteAt i (Seq t) =
  let (t1, t2) = split i t
   in case t2 of
        Nil -> Seq t
        _ -> Seq $ append t1 $ FingerTree.tail t2

insertAt :: Measured a => Int -> a -> Sequence a -> Sequence a
insertAt i x (Seq t) =
  let (t1, t2) = split i t
   in case t2 of
        Nil -> Seq $ insertTail x t
        _ -> Seq $ append t1 $ insertHead x t2

fromList :: Measured a => [a] -> Sequence a
fromList = Seq . FingerTree.fromList

toList :: Measured a => Sequence a -> [a]
toList (Seq t) = FingerTree.toList t

null :: Eq a => Measured a => Sequence a -> Bool
null (Seq t) = t == Nil

length :: Measured a => Sequence a -> Int
length (Seq t) = measure t

-- Given an index, split a sequence into a list of two sequenes at index
seqSplit :: Measured a => Sequence a -> Int -> [Sequence a]
seqSplit (Seq t) i =
  let (t1, t2) = FingerTree.split i t
   in [Seq t1, Seq t2]

-- Given an index, get elt stored at that index, if there is one
lookup :: Eq a => Measured a => Int -> Sequence a -> Maybe a
lookup x (Seq t) =
  let (_, t2) = split x t
   in if t2 /= Nil
        then FingerTree.head t2
        else Nothing

instance (Show a, Arbitrary a, Measured a) => Arbitrary (Sequence a) where
  arbitrary = fmap Seq FingerTree.arbitrary

  shrink :: Sequence a -> [Sequence a]
  shrink (Seq t) = fmap Seq (FingerTree.shrink t)