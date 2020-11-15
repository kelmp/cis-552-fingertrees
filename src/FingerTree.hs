{-# LANGUAGE InstanceSigs #-}

module FingerTree(FingerTree, insertHead, insertTail, head, tail,
                  isEmpty, concat, split, map, toList) where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Functor
import Data.Monoid
import Data.Traversable

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

instance Monoid FingerTree where
  mempty :: FingerTree a
  mempty = undefined

  (<>) :: FingerTree a -> FingerTree a -> FingerTree a  
  t1 <> t2 = undefined

  mconcat :: [FingerTree a] -> FingerTree a
  mconcat = undefined

instance Foldable FingerTree where
  foldMap :: Monoid m => (a -> m) -> FingerTree a -> m
  foldMap f t = undefined

  foldr :: (a -> b -> b) -> b -> FingerTree a -> b
  folr f b t = undefined

instance Traversable Tree where
  traverse :: Applicative z => (a -> z b) -> FingerTree a -> z (FingerTree b)
  traverse f t = undefined

------ Functions ------

insertHead :: a -> FingerTree a -> FingerTree a
insertHead x t = undefined

insertTail :: a -> FingerTree a -> FingerTree a
insertTail x t = undefined

head :: FingerTree a -> a
head t = undefined

tail :: FingerTree a -> a
tail t = undefined

isEmpty :: FingerTree a -> Bool
isEmpty t = undefined

concat :: FingerTree a -> FingerTree a -> FingerTree a
concat t1 t2 = undefined

split :: FingerTree a -> (FingerTree a, FingerTree a)
split t = undefined

map :: (a -> b) -> FingerTree a -> FingerTree b
map f t = undefined

toList :: FingerTree a -> [a]
toList t = undefined