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
  return x = undefined

  (>>=) :: Sequence a -> (a -> Sequence b) -> Sequence b
  t >>= f = undefined

instance Functor Sequence where
  fmap :: (a -> b) -> Sequence a -> Sequence b
  fmap f t = undefined

instance Applicative Sequence where
  pure :: a -> Sequence a
  pure t = undefined

  (<*>) :: Sequence (a -> b) -> Sequence a -> Sequence b
  t1 <*> t2 = undefined

instance Monoid (Sequence a) where
  mempty :: Sequence a
  mempty = undefined

instance Semigroup (Sequence a) where
  (<>) = undefined

instance Foldable Sequence where
  foldMap :: Monoid m => (a -> m) -> Sequence a -> m
  foldMap f t = undefined

  foldr :: (a -> b -> b) -> b -> Sequence a -> b
  foldr f b t = undefined

instance Traversable Sequence where
  traverse :: Applicative z => (a -> z b) -> Sequence a -> z (Sequence b)
  traverse f t = undefined

------ Functions ------

empty :: Sequence a
empty = undefined

singleton :: a -> Sequence a
singleton = undefined

(<|) :: a -> Sequence a -> Sequence a
(<|) = undefined

(|>) :: Sequence a -> a -> Sequence a
(|>) = undefined

(><) :: Sequence a -> Sequence a -> Sequence a
(><) = undefined

fromList :: [a] -> Sequence a
fromList = undefined

null :: Bool
null = undefined

length :: Int
length = undefined

lookup :: Int -> Sequence a -> Maybe a
lookup = undefined

index :: Sequence a -> Int -> a
index = undefined