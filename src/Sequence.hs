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

newtype Sequence a = Seq (FingerTree a)
  deriving (Eq, Show)

-- Instances --

-- instance Monad Sequence where
--   return :: a -> Sequence a
--   return = pure

--   (>>=) :: Sequence a -> (a -> Sequence b) -> Sequence b
--   (Seq t) >>= f = Seq (FingerTree . (>>=) t f)

-- foldl add empty t
--  where
--   add x y = x >< f y

-- instance Measured a => Functor (Sequence a) where
--   fmap :: (a -> b) -> Sequence a -> Sequence b
--   fmap f (Seq t) = Seq (FingerTree.fmap' f t)

-- instance Applicative Sequence where
--   pure :: a -> Sequence a
--   pure = singleton

--   (<*>) :: Sequence (a -> b) -> Sequence a -> Sequence b
--   (Seq t1) <*> (Seq t2) = Seq (FingerTree.(<*>) t1 t2)

instance Measured a => Monoid (Sequence a) where
  mempty :: Sequence a
  mempty = Seq Nil

instance Measured a => Semigroup (Sequence a) where
  (Seq t1) <> (Seq t2) = Seq ((<>) t1 t2)

-- instance Foldable Sequence where
--   foldMap :: Monoid m => (a -> m) -> Sequence a -> m
--   foldMap f (Seq t) = Seq (foldMap f t)

-- instance Traversable Sequence where
--   traverse :: Applicative z => (a -> z b) -> Sequence a -> z (Sequence b)
--   traverse f t = FingerTree.traverse f t

------ Functions ------

empty :: Sequence a
empty = Seq Nil

singleton :: Measured a => a -> Sequence a
singleton x = Seq $ Unit x

(<|) :: Measured a => a -> Sequence a -> Sequence a
(<|) x (Seq t) = Seq (insertHead x t)

(|>) :: Measured a => Sequence a -> a -> Sequence a
(|>) (Seq t) x = Seq (insertTail x t)

(><) :: Measured a => Sequence a -> Sequence a -> Sequence a
(><) (Seq t1) (Seq t2) = Seq (append t1 t2)

first :: Sequence a -> Maybe a
first (Seq t1) = FingerTree.head t1

last :: Sequence a -> Maybe a
last (Seq t1) = FingerTree.last t1

-- deleteLast :: Sequence a -> Sequence a
-- deleteLast (Seq t) = Seq (removeTail t)

deleteAt :: Measured a => Int -> Sequence a -> Sequence a
deleteAt i (Seq t) =
  let (t1, t2) = split i t
   in case t2 of
        Nil -> Seq t
        _ -> Seq $ append t1 $ FingerTree.tail t2

insertAt :: Measured a => Int -> a -> Sequence a -> Sequence a
insertAt = undefined

fromList :: Measured a => [a] -> Sequence a
fromList = Seq . FingerTree.fromList

toList :: Measured a => Sequence a -> [a]
toList (Seq t) = FingerTree.toList t

null :: Eq a => Measured a => Sequence a -> Bool
null (Seq t) = t == Nil

length :: Measured a => Sequence a -> Int
length (Seq t) = measure t

lookup :: Eq a => Measured a => Int -> Sequence a -> Maybe a
lookup x (Seq t) =
  let (_, t2) = split x t
   in if t2 /= Nil then FingerTree.head t2 else Nothing